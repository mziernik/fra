package com.database;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.utils.CSV;
import com.utils.text.StrWriter;
import com.exceptions.SQLError;
import com.utils.collections.Strings;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.drivers.h2.H2;
import com.mlogger.Log;
import com.database.DBConnectionData.DbConnections;
import com.database.DBConnectionData.DbLock;
import com.database.drivers.DbMeta;
import com.database.queries.*;
import com.database.queries.builder.QueryObject;
import com.exceptions.ThrowableException;
import com.io.IOUtils;
import com.lang.LDatabase;
import com.mlogger.*;
import com.model.dao.DatabaseDAO;
import com.model.dao.core.DAOQuery;
import com.resources.core.ResData;
import com.servlet.Handlers;
import com.utils.Undefined;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.io.*;
import java.net.URL;
import java.sql.*;
import java.util.*;

/**
 *
 * @author admin
 */
public abstract class Database extends DatabaseDAO {

    public boolean logsEnabled = true;
    public final DbMeta meta;
    protected DBConnectionData connData;
    protected final static DbConnections connections = new DbConnections();
    protected String dateFormat = "yyyy-MM-dd HH:mm:ss.SSS";
    DbLock transaction;
    boolean transactionDone = false;

    public Database(DBConnectionData connData, Class<? extends DbMeta> meta) {
        this.connData = connData;
        try {
            this.meta = meta.getDeclaredConstructor(Database.class).newInstance(this);
        } catch (Error | RuntimeException ex) {
            throw ex;
        } catch (Throwable ex) {
            throw new ThrowableException(ex);
        }
    }

    @Override
    public String toString() {
        return Utils.toString(connData);
    }

    /**
     * Wykonaj procedurę w transakcji
     *
     * @param <Exception>
     * @param caller
     * @throws Exception
     * @throws Exception
     * @throws SQLException
     */
    public Database transaction(Transaction caller) throws SQLException {

        boolean use = transaction == null;
        if (use)
            beginTransaction();
        try {
            caller.run(this);
            use &= !transactionDone;
            if (use)
                commitTransaction();
        } catch (Throwable e) {
            use &= !transactionDone;
            if (use)
                rollbackTransaction();
            throw e instanceof SQLException ? (SQLException) e : new SQLException(e);
        }
        return this;
    }

    public void beginTransaction() throws SQLException {
        connData.disconnectOnError = false;
        if (transaction != null)
            throw new SQLException(LDatabase.TRANSACTION_ALREADY_STARTED.toString());
        transaction = getLock("Transaction", 0);
        transaction.connection.setAutoCommit(false);
        transactionDone = false;
    }

    public void commitTransaction() throws SQLException {
        if (transaction == null)
            throw new SQLException(LDatabase.TRANSACTION_NOT_STARTED.toString());
        try {
            transaction.connection.commit();
        } finally {
            transaction.unlock();
            transaction = null;
            transactionDone = true;
        }
    }

    public void rollbackTransaction() throws SQLException {
        if (transaction == null)
            throw new SQLException(LDatabase.TRANSACTION_NOT_STARTED.toString());

        if (!transaction.connection.isClosed())
            transaction.connection.rollback();
        transaction.unlock();
        Log.warning("Database", "Wycofano transakcję");
        transaction = null;
        transactionDone = true;
    }

    public boolean isTransaction() {
        return transaction != null;
    }

    public static interface ISqlError {

        public abstract void onSqlError(SQLException ex, String query);
    }
    public ISqlError errorHandler;

    public boolean isPostgreSQL() {
        return this instanceof PostgreSQL;
    }

    public boolean isH2() {
        return this instanceof H2;
    }

    public DbLock getLock(String query, int lockTimeout) throws SQLException {
        String name = Utils.cutLongName(query, 50, false)
                .replaceAll("\n", " ")
                .replaceAll("\r", "");
        return connections.lock(this, connData(), lockTimeout, name);
    }

    public DBConnectionData connData() throws SQLException {
        if (connData == null)
            connData = Handlers.database.getInstance().getConnectionData(this);
        return connData;
    }

    /**
     * Zwraca typ UNDEFINED jesli obiejt jest NULLem albo jest pusty
     *
     * @param obj
     * @return
     */
    public static Object nonEmpty(Object obj) {
        return obj == null
                || obj.toString() == null
                || obj.toString().isEmpty()
                ? Undefined.TYPE : obj;
    }

    protected void initialize() throws SQLException {

    }

    public QueryRows execute(String query, Object... params) throws SQLException {
        return doExecute(null, query, null, params);
    }

    public QueryRows execute(URL query, Object... params) throws IOException, SQLException {
        return query != null ? doExecute(null, IOUtils.read(query, Utils.UTF8),
                StrUtils.decodeURIComponent(query.getFile()), params) : null;
    }

    public QueryRows execute(ResData query, Object... params) throws IOException, SQLException {
        return doExecute(null, query.getStringUtf8(), query.getFileName(), params);
    }

    protected QueryRows doExecute(DAOQuery daoQ, String query, String fileName, Object... params) throws SQLException {

        if (query == null)
            return null;

        long timestamp = System.nanoTime();

        StringWriter csvWriter = new StringWriter();
        connData(); // inicjalizacja

        if (params != null && params.length > 0)
            query = processParams(query, params);

        QueryRows result = null;

        DbLock lock = transaction != null ? transaction
                : getLock(connData.toString(), connData().lockTimeout);
        Exception exception = null;

        final Strings messages = new Strings();
        final Map<String, String> attrs = new LinkedHashMap<>();
        final Map<String, String> data = new LinkedHashMap<>();

        String details = null;
        SQLError se = null;

        Log log = new Log(LogKind.QUERY)
                .tag("SQL")
                .tag(connData.driverName);

        if (!Is.empty(fileName))
            log.value(fileName).details(query);
        else
            log.value(query);

        try (Statement stmt = lock.connection.createStatement()) {

            timestamp = System.nanoTime();
            try {
                boolean execute = stmt.execute(query);
            } finally {
                SQLWarning warns = stmt.getWarnings();
                if (warns != null)
                    for (Throwable w : warns)
                        messages.add(w.getLocalizedMessage());
            }
            result = new QueryRows(this, daoQ, stmt, stmt.getResultSet(), query, timestamp);

            if (lock.connection.isClosed())
                return result;

            logResults(result, csvWriter);

            return result;

        } catch (SQLException e) {

            String errorLine = null;
            /*
            if (e instanceof PSQLException) {
                ServerErrorMessage sem = ((PSQLException) e).getServerErrorMessage();
                if (sem != null) {
                    attrs.put("Column", sem.getColumn());
                    attrs.put("Constraint", sem.getConstraint());
                    attrs.put("Datatype", sem.getDatatype());
                    attrs.put("Detail", sem.getDetail());
                    attrs.put("File", sem.getFile());
                    attrs.put("Hint", sem.getHint());
                    attrs.put("InternalQuery", sem.getInternalQuery());
                    attrs.put("Message", sem.getMessage());
                    attrs.put("Routine", sem.getRoutine());
                    attrs.put("SQLState", sem.getSQLState());
                    attrs.put("Schema", sem.getSchema());
                    attrs.put("Table", sem.getTable());
                    attrs.put("Where", sem.getWhere());
                    attrs.put("Line", Integer.toString(sem.getLine()));
                    attrs.put("Position", Integer.toString(sem.getPosition()));
                    errorLine = getQueryErrorLine(query, sem.getPosition(), log);

                    if (sem.getRoutine() != null)
                        log.tag(sem.getRoutine());
                }
            }
             */
            for (Throwable t : e)
                if (t != e)
                    Log.warning("SQL", t);

            exception = e;
            if (errorHandler != null && (e instanceof SQLException))
                errorHandler.onSqlError((SQLException) e, query);

            se = new SQLError(e);

            String msg = e.toString();
            if (msg == null || !msg.contains(query))
                se.query(query);

            if (errorLine != null)
                se.details(LDatabase.LINE.toString(), errorLine);

            if (!messages.isEmpty()) {
                data.put(LDatabase.MESSAGES.toString(), messages.toString("\n"));
                se.details(LDatabase.MESSAGES.toString(), messages.toString("\n"));
            }
            se.attributes.putAll(attrs);
            se.data.putAll(data);

        } finally {
            if (result == null)
                result = new QueryRows(this, daoQ, null, null, query, timestamp);

            if (logsEnabled) {

                log.data("Rezultat", csvWriter.toString());
                log.attribute("Baza", lock.config.toString());
                log.attribute("Id połączenia", lock.id);
                log.attribute("Transakcja", Utils.boolToStr(transaction != null));

                QueryRows rows = result;
                int results = 1;

                if (rows.generatedKeys != null && !rows.generatedKeys.isEmpty())
                    for (QueryRow row : rows.generatedKeys)
                        for (QueryCell cell : row.getValues())
                            log.attribute("Wygenerowane klucze", cell.column.name, cell.value);

                String time = new Interval(result.executeTime, Unit.NANOSECONDS).toString();

                log.attribute("Czas wykonania", time)
                        .comment(time);

                while (rows != null) {
                    log.attribute("Rezultat" + (result.nextResults() != null
                            ? " " + results++ : ""),
                            new Strings()
                                    .add(rows.resultsCount != null
                                            ? "Zwrócono wierszy " + rows.resultsCount
                                            : null)
                                    .add(rows.updateCount != null
                                            ? "Zaktualizowano wierszy " + rows.updateCount
                                            : null)
                                    .toString(", "));
                    rows = rows.nextResults();
                }

                if (details != null)
                    for (Map.Entry<String, String> en : data.entrySet())
                        log.data(en.getKey(), en.getValue());

                log.send();

            }
            if (lock != transaction)
                lock.unlock();
            if (exception != null && connData != null && connData.disconnectOnError) {
                if (transaction != null)
                    rollbackTransaction();
                lock.disconnect(true);
            }
        }

        if (se != null)
            throw se;

        return null;
    }

    private String getQueryErrorLine(String query, int pos, Log log) {
        if (pos <= 0 || pos > query.length())
            return null;

        pos -= 1;
        int from = pos;
        int to = pos;

        char[] arr = query.toCharArray();

        for (int i = pos - 1; i >= 0; i--) {
            if (arr[i] == ' ')
                from = i + 1;
            if (arr[i] == '\n') {
                from = i + 1;
                break;
            }
        }

        for (int i = pos + 1; i < arr.length; i++) {
            if (arr[i] == ' ')
                to = i - 1;
            if (arr[i] == '\n') {
                to = i - 1;
                break;
            }
        }
        return new String(arr, from, to - from);
    }

    protected void logResults(QueryRows result, StringWriter csvWriter) {

        if (logsEnabled) {
            int len = 0;
            CSV.CSVWriter csv = new CSV.CSVWriter(csvWriter);
            try {
                List<String> colNames = new LinkedList<>();
                for (QueryColumn col : result.columns)
                    colNames.add(col.name);

                csv.writeNext(colNames.toArray(new String[0]));

                for (QueryRow qr : result) {
                    Strings lst = new Strings();
                    for (String s : qr.asArray()) {
                        if (s == null)
                            s = "<null>";

                        s = Utils.cutLongName(s, 100, false);
                        lst.add(s);
                        len += s.length() + 3;
                    }
                    csv.writeNext(lst.toArray());

                    if (len > 7000)
                        break;
                }

            } catch (Exception e) {
                Log.error(e);
            }
        }
    }

    public MultipleQuery multipleQuery() {
        return new MultipleQuery(this);
    }

    /*
     public List<String> getSchemas() throws SQLException {
     List<String> list = new LinkedList<>();

     long nanoTime = System.nanoTime();
     DatabaseMetaData meta = getMetaData();
     QueryRows rows = new QueryRows(null, meta.getSchemas(), "getSchemas()", nanoTime);
     for (QueryRow row : rows) {
     list.add(row.getStr("SCHEMA_NAME", ""));
     }
     return list;
     }
     */
    public DatabaseMetaData getMetaData() throws SQLException {
        DbLock lock = getLock("getMetaData()", 30000);
        try {
            return lock.connection.getMetaData();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Metoda podmienia parametry zapytania. %s - podstawiane jest zaescapowane
     * wyrazenie (bez cudzyslowia) ? - podmienia na escapowanie E'' dla
     * postgresa lub standarodwe dla pozostalych (z cudzyslowem)
     *
     * @param query
     * @param params
     * @return
     */
    public String processParams(final String query, final Object... params) throws SQLError {

        //ToDo: Sprawdzić PreparedStatement escape
        if (query == null || params == null || params.length == 0)
            return query;

        List<Object> lst = new LinkedList<>();
        for (Object o : params)
            if (o != Undefined.TYPE && o != Undefined.class)
                lst.add(o);

        final Object[] pars = lst.toArray(new Object[lst.size()]);

        if (pars.length == 0 || !query.contains("?"))
            return query;

        int count = 0;
        int lastIndex = 0;

        boolean allParamsNulls = true;

        for (Object o : pars)
            allParamsNulls &= (o == null);

        while (lastIndex != -1) {
            lastIndex = query.indexOf("?", lastIndex);

            if (lastIndex == -1)
                break;

            lastIndex += 1;
            count++;
        }

        if (pars.length > count && !allParamsNulls)
            Log.warning("Zbyt dużo parametrów zapytania")
                    .details("Oczekiwane " + count + ", aktualne " + pars.length)
                    .data("Zapytanie", query);

        if (pars.length < count)
            throw new SQLError(LDatabase.INVALID_NUMBER_OF_ARGS.toString(count, pars.length)).query(query);

        StrWriter out = new StrWriter();
        int paramIdx = 0;

        lastIndex = 0;
        while (lastIndex != -1) {
            int idx = query.indexOf("?", lastIndex);

            if (idx == -1) {
                out.append(query.substring(lastIndex));
                break;
            }

            out.append(query.subSequence(lastIndex, idx));

            out.append(new QueryObject(this, pars[paramIdx++])
                    .cast(null)
                    .escape(null)
                    .array(false)
                    .getEscapedValue());

            lastIndex = idx + 1;
        }
        return out.toString();

    }

    public Query query(String query, Object... params) {
        return new Query(this, query, params);
    }

    public Insert insert(String table) {
        return new Insert(this, table);
    }

    public InsertMultiple insertMultiple(String table, String... columns) throws SQLError {
        return new InsertMultiple(this, table, columns);
    }

    public Update update(String table, String where, Object... params) throws SQLError {
        return new Update(this, table, processParams(where, params));
    }

    public UpdateMultiple updateMultiple(String table, String keyColumn, String... columns) throws SQLError {
        return new UpdateMultiple(this, table, keyColumn, columns);
    }

    public InsertOrUpdate insertOrUpdate(String table, String where, Object... params) throws SQLError {
        return new InsertOrUpdate(this, table, processParams(where, params));
    }

    public StoredProc storedProc(String procName) {
        return new StoredProc(this, procName);
    }

    public Merge merge(String table, String keyColumn) {
        return new Merge(this, table, keyColumn);
    }

    /**
     * Podstawowe escapowanie, bez cudzysłowów
     *
     * @param str
     * @return
     */
    public static String escapeSQL(String str) {

        String res = str;
        if (res == null)
            return res;
        //    res = res.replace("\\", "\\\\");
        res = res.replace("'", "''");
        //    res = res.replace("\"", "\\\"");
        return res;
    }

//    @SuppressWarnings("unchecked")
//    public void checkStructure(Class<? extends ITable>... tables) throws SQLException {
//        if (tables == null || tables.length == 0)
//            return;
//
//        List<HTable> htables = new LinkedList<>();
//        for (Class<? extends ITable> itable : tables)
//            htables.add(new HTable(this, itable, null, false));
//
//        checkStructure(htables);
//    }
    // public abstract long getDatabaseSize(String databaseName) throws SQLException;
    // public abstract List<Pair<String, Long>> getTabsesSize(String... tables) throws SQLException;
    // public abstract void checkStructure(List<HTable> tables) throws SQLException;
    /**
     * Escapuje zapytanie i zwraca samego siebie
     *
     * @param qry
     * @param obj
     * @return
     */
    public abstract StrWriter escape(StrWriter qry, QueryObject obj);

    public String escape(Object value) {
        return escape(new StrWriter(), value instanceof QueryObject
                ? (QueryObject) value
                : new QueryObject(this, value))
                .toString();
    }

//    public String escapeUnquoted(Object value) {
//        return escape(new QueryObject(this, value).quote(false));
//    }
//    public String escape(QueryObject obj) {
//        return escape(new StringWriter(), obj, 0).toString();
//    }
    public final static String[] RESERVED = {
        "ABORT", "ABS", "ABSOLUTE", "ACCESS", "ACTION", "ADA", "ADD", "ADMIN",
        "AFTER", "AGGREGATE", "ALIAS", "ALL", "ALLOCATE", "ALSO", "ALTER", "ALWAYS",
        "ANALYSE", "ANALYZE", "AND", "ANY", "ARE", "ARRAY", "AS", "ASC", "ASENSITIVE",
        "ASSERTION", "ASSIGNMENT", "ASYMMETRIC", "AT", "ATOMIC", "ATTRIBUTE",
        "ATTRIBUTES", "AUTHORIZATION", "AVG", "BACKWARD", "BEFORE", "BEGIN",
        "BERNOULLI", "BETWEEN", "BIGINT", "BINARY", "BIT", "BITVAR", "BIT_LENGTH",
        "BLOB", "BOOLEAN", "BOTH", "BREADTH", "BY", "C", "CACHE", "CALL", "CALLED",
        "CARDINALITY", "CASCADE", "CASCADED", "CASE", "CAST", "CATALOG", "CATALOG_NAME",
        "CEIL", "CEILING", "CHAIN", "CHAR", "CHARACTER", "CHARACTERISTICS",
        "CHARACTERS", "CHARACTER_LENGTH", "CHARACTER_SET_CATALOG", "CHARACTER_SET_NAME",
        "CHARACTER_SET_SCHEMA", "CHAR_LENGTH", "CHECK", "CHECKED", "CHECKPOINT",
        "CLASS", "CLASS_ORIGIN", "CLOB", "CLOSE", "CLUSTER", "COALESCE", "COBOL",
        "COLLATE", "COLLATION", "COLLATION_CATALOG", "COLLATION_NAME",
        "COLLATION_SCHEMA", "COLLECT", "COLUMN", "COLUMN_NAME", "COMMAND_FUNCTION",
        "COMMAND_FUNCTION_CODE", "COMMENT", "COMMIT", "COMMITTED", "COMPLETION",
        "CONDITION", "CONDITION_NUMBER", "CONNECT", "CONNECTION", "CONNECTION_NAME",
        "CONSTRAINT", "CONSTRAINTS", "CONSTRAINT_CATALOG", "CONSTRAINT_NAME",
        "CONSTRAINT_SCHEMA", "CONSTRUCTOR", "CONTAINS", "CONTINUE", "CONVERSION",
        "CONVERT", "COPY", "CORR", "CORRESPONDING", "COUNT", "COVAR_POP",
        "COVAR_SAMP", "CREATE", "CREATEDB", "CREATEROLE", "CREATEUSER", "CROSS",
        "CSV", "CUBE", "CUME_DIST", "CURRENT", "CURRENT_DATE",
        "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH", "CURRENT_ROLE",
        "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_TRANSFORM_GROUP_FOR_TYPE",
        "CURRENT_USER", "CURSOR", "CURSOR_NAME", "CYCLE", "DATA", "DATABASE",
        "DATE", "DATETIME_INTERVAL_CODE", "DATETIME_INTERVAL_PRECISION", "DAY",
        "DEALLOCATE", "DEC", "DECIMAL", "DECLARE", "DEFAULT", "DEFAULTS",
        "DEFERRABLE", "DEFERRED", "DEFINED", "DEFINER", "DEGREE", "DELETE",
        "DELIMITER", "DELIMITERS", "DENSE_RANK", "DEPTH", "DEREF", "DERIVED",
        "DESC", "DESCRIBE", "DESCRIPTOR", "DESTROY", "DESTRUCTOR", "DETERMINISTIC",
        "DIAGNOSTICS", "DICTIONARY", "DISABLE", "DISCONNECT", "DISPATCH",
        "DISTINCT", "DO", "DOMAIN", "DOUBLE", "DROP", "DYNAMIC", "DYNAMIC_FUNCTION",
        "DYNAMIC_FUNCTION_CODE", "EACH", "ELEMENT", "ELSE", "ENABLE", "ENCODING",
        "ENCRYPTED", "END", "END-EXEC", "EQUALS", "ESCAPE", "EVERY", "EXCEPT",
        "EXCEPTION", "EXCLUDE", "EXCLUDING", "EXCLUSIVE", "EXEC", "EXECUTE",
        "EXISTING", "EXISTS", "EXP", "EXPLAIN", "EXTERNAL", "EXTRACT", "FALSE",
        "FETCH", "FILTER", "FINAL", "FIRST", "FLOAT", "FLOOR", "FOLLOWING", "FOR",
        "FORCE", "FOREIGN", "FORTRAN", "FORWARD", "FOUND", "FREE", "FREEZE",
        "FROM", "FULL", "FUNCTION", "FUSION", "G", "GENERAL", "GENERATED", "GET",
        "GLOBAL", "GO", "GOTO", "GRANT", "GRANTED", "GREATEST", "GROUP",
        "GROUPING", "HANDLER", "HAVING", "HEADER", "HIERARCHY", "HOLD",
        "HOST", "HOUR", "IDENTITY", "IGNORE", "ILIKE", "IMMEDIATE", "IMMUTABLE",
        "IMPLEMENTATION", "IMPLICIT", "IN", "INCLUDING", "INCREMENT", "INDEX",
        "INDICATOR", "INFIX", "INHERIT", "INHERITS", "INITIALIZE", "INITIALLY",
        "INNER", "INOUT", "INPUT", "INSENSITIVE", "INSERT", "INSTANCE",
        "INSTANTIABLE", "INSTEAD", "INT", "INTEGER", "INTERSECT", "INTERSECTION",
        "INTERVAL", "INTO", "INVOKER", "IS", "ISNULL", "ISOLATION",
        "ITERATE", "JOIN", "K", "KEY", "KEY_MEMBER", "KEY_TYPE", "LANCOMPILER",
        "LANGUAGE", "LARGE", "LAST", "LATERAL", "LEADING", "LEAST", "LEFT",
        "LENGTH", "LESS", "LEVEL", "LIKE", "LIMIT", "LISTEN", "LN", "LOAD",
        "LOCAL", "LOCALTIME", "LOCALTIMESTAMP", "LOCATION", "LOCATOR",
        "LOCK", "LOGIN", "LOWER", "M", "MAP", "MATCH", "MATCHED", "MAX",
        "MAXVALUE", "MEMBER", "MERGE", "MESSAGE_LENGTH", "MESSAGE_OCTET_LENGTH",
        "MESSAGE_TEXT", "METHOD", "MIN", "MINUTE", "MINVALUE", "MOD", "MODE",
        "MODIFIES", "MODIFY", "MODULE", "MONTH", "MORE", "MOVE", "MULTISET",
        "MUMPS", "NAME", "NAMES", "NATIONAL", "NATURAL", "NCHAR", "NCLOB",
        "NESTING", "NEW", "NEXT", "NO", "NOCREATEDB", "NOCREATEROLE", "NOCREATEUSER",
        "NOINHERIT", "NOLOGIN", "NONE", "NORMALIZE", "NORMALIZED", "NOSUPERUSER",
        "NOT", "NOTHING", "NOTIFY", "NOTNULL", "NOWAIT", "NULL", "NULLABLE",
        "NULLIF", "NULLS", "NUMBER", "NUMERIC", "OBJECT", "OCTETS", "OCTET_LENGTH",
        "OF", "OFF", "OFFSET", "OIDS", "OLD", "ON", "ONLY", "OPEN", "OPERATION",
        "OPERATOR", "OPTION", "OPTIONS", "OR", "ORDER", "ORDERING", "ORDINALITY",
        "OTHERS", "OUT", "OUTER", "OUTPUT", "OVER", "OVERLAPS", "OVERLAY",
        "OVERRIDING", "OWNER", "PAD", "PARAMETER", "PARAMETERS", "PARAMETER_MODE",
        "PARAMETER_NAME", "PARAMETER_ORDINAL_POSITION", "PARAMETER_SPECIFIC_CATALOG",
        "PARAMETER_SPECIFIC_NAME", "PARAMETER_SPECIFIC_SCHEMA", "PARTIAL", "PARTITION",
        "PASCAL", "PASSWORD", "PATH", "PERCENTILE_CONT", "PERCENTILE_DISC",
        "PERCENT_RANK", "PLACING", "PLI", "POSITION", "POSTFIX", "POWER",
        "PRECEDING", "PRECISION", "PREFIX", "PREORDER", "PREPARE", "PREPARED",
        "PRESERVE", "PRIMARY", "PRIOR", "PRIVILEGES", "PROCEDURAL", "PROCEDURE",
        "PUBLIC", "QUOTE", "RANGE", "RANK", "READ", "READS", "REAL", "RECHECK",
        "RECURSIVE", "REF", "REFERENCES", "REFERENCING", "REGR_AVGX", "REGR_AVGY",
        "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX",
        "REGR_SXY", "REGR_SYY", "REINDEX", "RELATIVE", "RELEASE", "RENAME",
        "REPEATABLE", "REPLACE", "RESET", "RESTART", "RESTRICT", "RESULT",
        "RETURN", "RETURNED_CARDINALITY", "RETURNED_LENGTH", "RETURNED_OCTET_LENGTH",
        "RETURNED_SQLSTATE", "RETURNS", "REVOKE", "RIGHT", "ROLE", "ROLLBACK",
        "ROLLUP", "ROUTINE", "ROUTINE_CATALOG", "ROUTINE_NAME", "ROUTINE_SCHEMA",
        "ROW", "ROWS", "ROW_COUNT", "ROW_NUMBER", "RULE", "SAVEPOINT",
        "SCALE", "SCHEMA", "SCHEMA_NAME", "SCOPE", "SCOPE_CATALOG", "SCOPE_NAME",
        "SCOPE_SCHEMA", "SCROLL", "SEARCH", "SECOND", "SECTION", "SECURITY",
        "SELECT", "SELF", "SENSITIVE", "SEQUENCE", "SERIALIZABLE", "SERVER_NAME",
        "SESSION", "SESSION_USER", "SET", "SETOF", "SETS", "SHARE", "SHOW", "SIMILAR",
        "SIMPLE", "SIZE", "SMALLINT", "SOME", "SOURCE", "SPACE", "SPECIFIC",
        "SPECIFICTYPE", "SPECIFIC_NAME", "SQL", "SQLCODE", "SQLERROR", "SQLEXCEPTION",
        "SQLSTATE", "SQLWARNING", "SQRT", "STABLE", "START", "STATE", "STATEMENT",
        "STATIC", "STATISTICS", "STDDEV_POP", "STDDEV_SAMP", "STDIN", "STDOUT",
        "STORAGE", "STRICT", "STRUCTURE", "STYLE", "SUBCLASS_ORIGIN", "SUBLIST",
        "SUBMULTISET", "SUBSTRING", "SUM", "SUPERUSER", "SYMMETRIC", "SYSID",
        "SYSTEM", "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TABLESPACE", "TABLE_NAME",
        "TEMP", "TEMPLATE", "TEMPORARY", "TERMINATE", "THAN", "THEN", "TIES",
        "TIME", "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TOAST",
        "TOP_LEVEL_COUNT", "TRAILING", "TRANSACTION", "TRANSACTIONS_COMMITTED",
        "TRANSACTIONS_ROLLED_BACK", "TRANSACTION_ACTIVE", "TRANSFORM", "TRANSFORMS",
        "TRANSLATE", "TRANSLATION", "TREAT", "TRIGGER", "TRIGGER_CATALOG",
        "TRIGGER_NAME", "TRIGGER_SCHEMA", "TRIM", "TRUE", "TRUNCATE", "TRUSTED",
        "TYPE", "UESCAPE", "UNBOUNDED", "UNCOMMITTED", "UNDER", "UNENCRYPTED",
        "UNION", "UNIQUE", "UNKNOWN", "UNLISTEN", "UNNAMED", "UNNEST", "UNTIL",
        "UPDATE", "UPPER", "USAGE", "USER", "USER_DEFINED_TYPE_CATALOG",
        "USER_DEFINED_TYPE_CODE", "USER_DEFINED_TYPE_NAME", "USER_DEFINED_TYPE_SCHEMA",
        "USING", "VACUUM", "VALID", "VALIDATOR", "VALUE", "VALUES", "VARCHAR",
        "VARIABLE", "VARYING", "VAR_POP", "VAR_SAMP", "VERBOSE", "VIEW", "VOLATILE",
        "WHEN", "WHENEVER", "WHERE", "WIDTH_BUCKET", "WINDOW", "WITH", "WITHIN",
        "WITHOUT", "WORK", "WRITE", "YEAR", "ZONE"};
}

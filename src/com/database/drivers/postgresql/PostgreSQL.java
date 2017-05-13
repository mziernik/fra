package com.database.drivers.postgresql;

import com.dev.Dev;
import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import com.context.*;
import com.database.DBConnectionData;
import com.database.Database;
import com.database.QueryRow;
import com.database.drivers.postgresql.PGNotifier.PGNotifierMessage;
import com.database.elements.JdbcDriver;
import com.database.queries.Upsert;
import com.database.queries.builder.*;
import com.json.*;
import com.utils.*;
import java.io.*;
import java.sql.*;
import java.text.SimpleDateFormat;

import static com.database.Database.escapeSQL;
import com.lang.LDatabase;

import com.utils.hashes.Base64;
import com.utils.reflections.TClass;
import java.util.*;

/**
 * Miłosz Ziernik 2014/06/20
 */
@JdbcDriver("org.postgresql.Driver")
public class PostgreSQL extends Database {

    // https://jdbc.postgresql.org/documentation/head/connect.html
    public Boolean eEscape = null;
    public final PgMeta meta;

    public PostgreSQL(String host, String dbName, String username, String password) {
        this(new DBConnectionData(PostgreSQL.class, "PostgreSQL", host + "/" + dbName)
                .property("user", username)
                .property("password", password));
    }

    public PostgreSQL(DBConnectionData connData) {
        super(connData, PgMeta.class);
        this.meta = (PgMeta) super.meta;

        if (connData != null && !connData.properties.contains("ApplicationName"))
            connData.properties.setProperty("ApplicationName", AppConfig.getServiceTitle());
    }

    public PostgreSQL() {
        super(null, PgMeta.class);
        this.meta = (PgMeta) super.meta;
    }

    public Upsert upsert(String table, String... keyColumns) {
        return new Upsert(this, table, keyColumns);
    }

    public PGNotifier addNotifier(PGNotifierMessage intf, String... notifyMessages) throws SQLException {
        return new PGNotifier(this, intf, notifyMessages);
    }

    /**
     * Zwraca wartość sekwencji (np typu serial)
     */
    public long nextVal(String sequence) throws SQLException {
        QueryRow row = execute("SELECT nextval(?::regclass)", sequence).first();
        if (row == null)
            throw new SQLException("Sekwencja '" + sequence + "' nie zwróciła wartości");
        return row.getInt(0, -1);
    }

    /*
     @Override
     public long getDatabaseSize(String databaseName) throws SQLException {
     return run("SELECT pg_database_size('" + databaseName + "')").first().getLong(0);
     }

     @Override
     public List<Pair<String, Long>> getTabsesSize(String... tables) throws SQLException {

     List<Pair<String, Long>> list = new LinkedList<>();
     for (QueryRow row : run("SELECT nspname || '.' || relname AS \"relation\",\n"
     + " pg_total_relation_size(C.oid) AS \"total_size\"\n"
     + " FROM pg_class C\n"
     + " LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)\n"
     + " WHERE nspname NOT IN ('pg_catalog', 'information_schema')\n"
     + " AND C.relkind <> 'i'\n"
     + " AND nspname !~ '^pg_toast'\n"
     + " ORDER BY pg_total_relation_size(C.oid) DESC\n"
     + " LIMIT 20;")) {

     String name = row.getStr(0);
     if (tables == null || tables.length == 0)
     list.add(new Pair<>(name, row.getLong("total_size")));
     else
     for (String s : tables)
     if (name.equalsIgnoreCase(s))
     list.add(new Pair<>(name, row.getLong("total_size")));
     }
     return list;
     }

     @Override
     public void checkStructure(List<HTable> tables) throws SQLException {
     throw new UnsupportedOperationException("Not supported yet.");
     }
     */
    @Override
    public StrWriter escape(StrWriter qry, QueryObject obj) {
        return doEscape(qry, obj, 0);
    }

    private StrWriter doEscape(StrWriter qry, QueryObject obj, int level) {

        Object value = obj.getValue();

        if (value == null)
            return qry.append("null");

        if (value == Undefined.TYPE)
            return qry; // pomiń wartość

        if (!obj.needEscape())
            return qry.append(value.toString());

        List<Object> array = null;
        try {

            if (value instanceof TObject)
                value = ((TObject) value).get();

            if (value instanceof byte[]) {
                qry.append("decode('")
                        .append(Base64.encode((byte[]) value))
                        .append("', 'base64')");
                obj.array(false);
                return qry;
            }

            if (value instanceof JElement) {
                obj.setIsCollection(false);
                value = value.toString();
            }

            if (value instanceof Map) {
                Map<?, ?> map = (Map) value;
                StrWriter sb = new StrWriter();
                for (Map.Entry<?, ?> en : map.entrySet()) {
                    if (sb.length() > 0)
                        sb.append(", ");
                    sb.append(Escape.escape(en.getKey()));
                    sb.append(" => ");
                    sb.append(Escape.escape(en.getValue()));
                }
                qry.append("'");
                qry.append(escapeSQL(sb.toString()));
                qry.append("'");
                return qry;
            }

            array = obj.isCollection() ? obj.asCollection() : null;

            if (array != null) {
                if (Boolean.TRUE.equals(obj.array()))
                    qry.append((level == 0 ? "ARRAY" : "") + "[");

                boolean first = true;
                for (Object o : array) {
                    if (!first)
                        qry.append(", ");

                    qry.nextLevel(() -> {
                        doEscape(qry, new QueryObject(this, o)
                                .array(true), level + 1);
                    });

                    first = false;
                }

                if (Boolean.TRUE.equals(obj.array()))
                    qry.append("]");

                return qry;
            }

            if (value instanceof java.util.Date)
                value = new SimpleDateFormat(dateFormat)
                        .format((java.util.Date) value);

            boolean pgEscape = Boolean.TRUE.equals(eEscape);

            if (pgEscape) { // określ, czy escapowanie jest wymagane
                pgEscape = false;
                for (char c : Utils.toString(value).toCharArray())
                    pgEscape |= (c < 32 && c != '\r' && c != '\n' && c != '\t');
            }

            if (pgEscape) {
                qry.append("E'");
                qry.append(new Escape().useQuota(false).toString(value));
                qry.append("'");
                return qry;
            }

            qry.append("'");
            qry.append(escapeSQL(Utils.toString(value)));
            qry.append("'");

        } finally {
            String cast = !Is.empty(obj.cast())
                    ? obj.cast()
                    : obj.cast() == null && obj.column() != null && !Is.empty(obj.column().cast)
                    ? obj.column().cast : null;

            // próba automatycznego okreslenia typu tablicy (jesli nie ma rzutowania)
            if (level == 0
                    && !Boolean.FALSE.equals(obj.array())
                    && array != null
                    && cast == null
                    && value != null) {

                Class<? extends Object> cls = value.getClass();

                if (cls.isArray())
                    cast = getSqlTypeName(cls.getComponentType());

                if (value instanceof Collection) {
                    Class[] gtypes = new TClass(cls).getClassGenericTypes();
                    if (gtypes.length == 1)
                        cast = getSqlTypeName(gtypes[0]);
                }

                if (!Is.empty(cast))
                    cast += "[]";
                else if (AppContext.devMode)
                    throw new RuntimeException("Dodano kolumnę " + (!Is.empty(obj.name())
                            ? Escape.unescape(obj.name()) : "")
                            + " bez rzutowania");
                else
                    Dev.warning("SQL", "Dodano kolumnę " + (!Is.empty(obj.name())
                            ? Escape.unescape(obj.name()) : "")
                            + " bez rzutowania");
            }

            if (cast != null)
                qry.append("::").append(cast);
        }
        return qry;
    }

    public static void backup(DBConnectionData data, List<String> databases, OutputStream out)
            throws IOException, SQLException, InterruptedException {
        /*
         File pgDump = new Path(data.binPath, "pg_dump"
         + (SystemProperties.isWindowsOS ? ".exe" : "")).getFile();


         if (!pgDump.exists())
         throw new FileNotFoundException("pg_dump");

         if (databases == null) {
         databases = new LinkedList<>();
         databases.add(data.database);
         }

         String host = data.host;
         if (host == null)
         return;

         if (host.contains(":"))
         host = host.substring(0, host.indexOf(":"));

         File tmp = File.createTempFile("pgbackup", null);
         try {
         ZipOutputStream zos = new ZipOutputStream(out);

         try {
         zos.setLevel(9);

         for (String database : databases) {

         ProcessBuilder pb = new ProcessBuilder(pgDump.getAbsolutePath(),
         "--host", host,
         "--port", "5432",
         "--username", data.username,
         "--no-password",
         "--format", "plain",
         "--inserts",
         // "--compress", "9",
         "--blobs",
         "--encoding", "UTF8",
         "--verbose",
         "--file", tmp.getAbsolutePath(),
         database);

         pb.environment().put("PGPASSWORD", data.password);
         pb.redirectErrorStream(true);
         StringBuilder res = new StringBuilder();

         Process p = pb.start();

         BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
         String ll;
         while ((ll = br.readLine()) != null) {
         res.append(ll);
         Log.debug("DB backup", ll);
         }

         p.waitFor();

         if (p.exitValue() != 0)
         throw new IOException(res.toString());

         ZipEntry ze = new ZipEntry(database
         + new SimpleDateFormat("_yyyyMMdd_HHmmss").format(new Date()) + ".sql");
         zos.putNextEntry(ze);
         IOUtils.copy(tmp, zos);

         zos.closeEntry();
         }

         } finally {
         zos.finish();
         zos.close();
         }
         } finally {
         tmp.delete();
         } */
    }

    public static String restore(DBConnectionData data, File f)
            throws SQLException, FileNotFoundException, IOException, InterruptedException {
        /*
         File psql = new Path(data.binPath, "psql"
         + (SystemProperties.isWindowsOS ? ".exe" : "")).getFile();
         if (!psql.exists())
         throw new FileNotFoundException("psql");

         List<String> databases = getDatabases(data);

         List<String> created = new LinkedList<>();
         Database db = customConnection(data);
         ZipInputStream zis = new ZipInputStream(new FileInputStream(f));
         ZipEntry entry;
         while ((entry = zis.getNextEntry()) != null)
         if (entry.getName().toLowerCase().endsWith(".sql")) {
         File tmp = File.createTempFile("pgbackup", null);
         try {

         IOUtils.copy(zis, tmp);

         String host = data.host;
         if (host.contains(":"))
         host = host.substring(0, host.indexOf(":"));

         String dbName = new Path(entry.getName()).getFileNameWithoutExt();

         if (dbName.length() > 15)
         if (dbName.charAt(dbName.length() - 7) == '_'
         && dbName.charAt(dbName.length() - 16) == '_') {
         String s = dbName.substring(0, dbName.length() - 16);

         if (!databases.contains(s))
         dbName = s.trim();

         }

         db.run("CREATE DATABASE " + dbName);

         ProcessBuilder pb = new ProcessBuilder(psql.getAbsolutePath(),
         "--host", host,
         "--port", "5432",
         "--username", data.username,
         "--no-password",
         "--file", tmp.getAbsolutePath(),
         "--dbname", dbName);

         pb.environment().put("PGPASSWORD", data.password);
         pb.redirectErrorStream(true);

         Process p = pb.start();

         BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
         String ll;
         String res = "";
         while ((ll = br.readLine()) != null) {
         res += ll + "\n";
         Log.debug("DB", ll);
         }

         p.waitFor();

         if (p.exitValue() != 0)
         throw new IOException(res);

         created.add(dbName);
         } finally {
         tmp.delete();
         }

         }

         if (created.isEmpty())
         throw new IOException("Nieprawidłowy format pliku");

         if (created.size() == 1)
         return "Utworzono bazę " + created.getIfNotNull(0);

         return "Utworzono bazy: " + Utils.listToString(created, ", ");
         */
        return null;
    }

    public String getSqlTypeName(Class<?> cls) {

        if (cls == Boolean.class)
            return "BOOL";

        if (cls == Character.class)
            return "CHAR";

        if (cls == String.class)
            return "VARCHAR";

        if (cls == byte.class || cls == Short.class)
            return "SMALLINT";

        if (cls == Integer.class)
            return "INT";

        if (Number.class.isAssignableFrom(cls))
            return "NUMERIC";

        if (java.util.Date.class.isAssignableFrom(cls))
            return "TIMESTAMP";

        return null;
    }

}

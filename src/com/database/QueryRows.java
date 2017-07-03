package com.database;

import com.utils.text.StrWriter;
import com.utils.hashes.Hex;
import com.utils.collections.Strings;
import com.mlogger.Log;
import com.utils.Utils;
import com.context.AppContext;
import com.dev.Dev;
import com.json.JArray;
import com.json.JObject;
import com.lang.LDatabase;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.utils.collections.TList;
import com.utils.text.Similarity;
import java.security.*;
import java.sql.*;
import java.util.*;

public class QueryRows extends DAORows<QueryRow> implements Iterable<QueryRow> {

    final TList<QueryRow> rows = new TList<>();
    public final long executeTime; // czas w nanosekundach
    public final String query;
    public final Integer updateCount;
    public final Integer resultsCount;
    public final String[] messages;
    public final Database db;
    private final QueryRows next;
    public final QueryRows generatedKeys;
    public final QueryColumn[] columns;
    public final DAOQuery daoQuery;

    @Override
    public String toString() {
        StrWriter sb = new StrWriter();
        Strings lst = new Strings();
        for (QueryColumn col : columns)
            lst.add(col.name);

        sb.append("0. ").append(lst.toString("; ")).append("\n");

        int idx = 0;
        for (QueryRow row : rows) {
            lst = new Strings();
            for (Object o : row.values)
                lst.add(o == null ? "<null>" : Utils.toString(o));
            sb.append(++idx + ". ").append(lst.toString("; ")).append("\n");
        }

        return sb.toString();
    }

    private QueryRows(QueryRows parent) {
        super(parent.daoQuery);
        daoQuery = parent.daoQuery;
        query = parent.query;
        executeTime = parent.executeTime;
        updateCount = parent.updateCount;
        resultsCount = parent.resultsCount;
        messages = parent.messages;
        db = parent.db;
        next = parent.next;
        generatedKeys = null;
        columns = parent.columns;
    }

    public QueryRows nextResults() {
        return next;
    }

    public LinkedList<QueryRows> getResults() {
        LinkedList<QueryRows> lst = new LinkedList<>();
        lst.add(this);
        QueryRows n = next;
        while (n != null) {
            lst.add(n);
            n = n.next;
        }
        return lst;
    }

    private ResultSet getNextResults(Statement stmt) throws SQLException {
        if (stmt == null)
            return null;

        int updCnt = stmt.getUpdateCount();
        boolean hasMore = stmt.getMoreResults();

        int counter = 0;
        while (!hasMore && updCnt >= 0 && counter < 100) {
            updCnt = stmt.getUpdateCount();
            hasMore = stmt.getMoreResults();
            ++counter;
        }

        return stmt.getResultSet();
    }

    public QueryRows(Database db, DAOQuery daoQuery, Statement stmt, ResultSet rst, String query,
            long timestamp) throws SQLException {

        super(daoQuery);

        this.daoQuery = daoQuery;
        this.query = query;
        this.db = db;

        if (stmt == null && rst != null)
            stmt = rst.getStatement();

        updateCount = stmt != null && stmt.getUpdateCount() >= 0 ? stmt.getUpdateCount() : null;

        List<String> msgs = new LinkedList<>();

        SQLWarning warns = stmt != null ? stmt.getWarnings() : null;
        if (warns != null)
            for (Throwable w : warns)
                msgs.add(w.getLocalizedMessage());

        this.messages = msgs.toArray(new String[0]);

        if (rst == null) {
            columns = new QueryColumn[0];
            executeTime = System.nanoTime() - timestamp;
            resultsCount = null;

            ResultSet nextResults = getNextResults(stmt);
            next = nextResults != null ? new QueryRows(db, daoQuery, stmt, nextResults, query, timestamp) : null;

            generatedKeys = stmt == null || updateCount == null || "#generatedKeys".equals(query)
                    ? null : new QueryRows(db, daoQuery, null, stmt.getGeneratedKeys(),
                            "#generatedKeys", timestamp);
            return;
        }

        try {

            ResultSetMetaData rsmd = rst.getMetaData();
            int colsCount = rsmd.getColumnCount();
            List<QueryColumn> cList = new LinkedList<>();
            for (int i = 0; i < colsCount; i++)
                cList.add(new QueryColumn(rsmd, i, this));

            columns = cList.toArray(new QueryColumn[0]);

            while (rst.next()) {
                Object[] cells = new Object[colsCount];
                for (int i = 0; i < colsCount; i++) {
                    Object o = rst.getObject(i + 1);
                    cells[i] = db.meta.formatObject(o);
                }
                rows.add(new QueryRow(cells, this));
            }

            executeTime = System.nanoTime() - timestamp;

            generatedKeys = stmt == null || updateCount == null || "#generatedKeys".equals(query)
                    ? null
                    : new QueryRows(db, daoQuery, null, stmt.getGeneratedKeys(),
                            "#generatedKeys", timestamp);
        } finally {
            rst.close();
        }

        resultsCount = rows.size();

        ResultSet nextResults = getNextResults(stmt);
        next = nextResults != null ? new QueryRows(db, daoQuery, stmt, nextResults, query, timestamp) : null;

    }

    /**
     * Zwraca pierwszy wiersz rezultatów. Jesli nie istnieje zwaracana jest
     * zaślepka;
     *
     * @return
     */
    public QueryRow firstD() {
        QueryRow row = first();
        return row != null ? row : QueryRow.getDummy(this);
    }

    /**
     * Zwraca pierwszy wiersz rezultatów lub null-a
     *
     * @return
     */
    public QueryRow first() {
        if (rows.isEmpty())
            return null;
        return rows.get(0);
    }

    public QueryRows find(String columnName, Object value) throws SQLException {
        QueryRows result = new QueryRows(this);
        QueryColumn column = getColumn(columnName);

        if (column == null)
            throw new SQLException(LDatabase.COLUMN_NOT_FOUND.toString(columnName));

        for (QueryRow row : this)
            if (Objects.equals(row.values[column.index], value))
                result.rows.add(row);

        return result;
    }

    public QueryRow findFirst(String columnName, Object value) throws SQLException {
        QueryRows rows = find(columnName, value);
        return rows != null ? rows.first() : null;
    }

    /**
     * Odnajduje rekord na podstawie nazwy kolumny i wartości. Jeśli rekordu nie
     * znaleziono, zwróconya zostanie zaslepka
     *
     * @param columnName
     * @param value
     * @return
     * @throws SQLException
     */
    public QueryRow findFirstD(String columnName, Object value) throws SQLException {
        QueryRows rows = find(columnName, value);
        QueryRow row = rows != null ? rows.first() : null;
        return row != null ? row : QueryRow.getDummy(this);
    }

    public int getNonEmptyCellsCount(String column) {
        QueryColumn col = getColumn(column);
        if (col == null)
            return 0;
        return col.getNonEmptyCellsCount();
    }

    /**
     * Zwraca zawartość wszystkich komórek danej kolumny
     */
    public List<Object> getContent(int column) {
        QueryColumn col = getColumn(column);
        if (col == null)
            return new LinkedList<>();
        return col.getContent();
    }

    /**
     * Zwraca zawartość dwóch kolumn w formie mapy
     */
    public <K, V> LinkedHashMap<K, V> getMap(String keyColumn, Class<K> keyClass,
            String valueColumn, Class<V> valueClass) throws SQLException {
        LinkedHashMap<K, V> map = new LinkedHashMap<>();
        for (QueryRow row : rows)
            map.put((K) row.getObj(keyColumn), (V) row.getObj(valueColumn, null));
        return map;
    }

    public List<Object> getContent(String column) throws SQLException {
        QueryColumn col = getColumn(column);
        if (col == null)
            throw new SQLException(LDatabase.COLUMN_NOT_FOUND.toString(column));

        return col.getContent();
    }

    public List<Number> getContentNumber(String column) throws SQLException {
        QueryColumn col = getColumn(column);
        if (col == null)
            throw new SQLException(LDatabase.COLUMN_NOT_FOUND.toString(column));
        return col.getContentNumber();
    }

    public QueryColumn getColumn(String name) {

        List<String> cols = new LinkedList<>();

        for (QueryColumn qc : columns) {
            cols.add(qc.name);
            if (qc.name.equalsIgnoreCase(name))
                return qc;
        }

        if (AppContext.devMode) {
            Strings lst = Similarity.getSimilars(name, cols.iterator(), 0.7d, 3)
                    .prefix("\"").sufix("\"").separator(" lub ");
            if (!lst.isEmpty())
                Dev.warning("SQL", LDatabase.COLUMN_NOT_FOUND.toString(name) + ". "
                        + LDatabase.PERHAPS_YOU_MEAN.toString(lst));

        }

        return null;
    }

    public QueryColumn getColumn(int number) {
        if (number >= 0 && number < columns.length)
            return columns[number];
        return null;
    }

    public boolean isEmpty() {
        return rows.isEmpty();
    }

    public int size() {
        return rows.size();
    }

    /**
     * Zwraca hash z wyników zapytania (przydatny do weryfikacji zmian)
     */
    public String getHash() {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            for (QueryRow row : rows)
                for (Object s : row.values)
                    if (s != null)
                        md.update(s.toString().getBytes("UTF-8"));
            return Hex.toString(md.digest());
        } catch (Exception ex) {
            Log.warning(ex);
            return "";
        }
    }

    /**
     * Grupuje rezultaty na podstawie nazwy kolumny
     *
     * @param columnName
     * @return
     * @throws SQLException
     */
    public Collection<QueryRows> groupBy(String columnName) throws SQLException {
        QueryColumn column = this.getColumn(columnName);
        Map<Object, QueryRows> map = new LinkedHashMap<>();
        if (column != null)
            for (QueryRow row : this) {
                Object key = row.getObj(column.index);
                QueryRows qrows = map.get(key);
                if (qrows == null) {
                    qrows = new QueryRows(this);
                    map.put(key, qrows);
                }
                qrows.rows.add(row);
                row.rows = qrows;
            }
        return map.values();
    }

    public LinkedList<QueryColumn> getColumns() {
        return Utils.asList(columns);
    }

    public QueryRows inverseOrder() {
        Collections.reverse(rows);
        return this;
    }

    public QueryRow get(int index) {
        return index >= 0 && index < rows.size() ? rows.get(index) : null;
    }

    /**
     * Zapiuje rezultaty do JArray
     *
     * @param compact zapisuje strukturę zbliżoną do CSV
     * @return
     * @throws SQLException
     */
    public JArray toJson(boolean compact) throws SQLException {
        JArray arr = new JArray();

        if (compact) {
            JArray hdr = arr.array();
            hdr.options.singleLine(true);

            for (QueryColumn col : columns)
                hdr.add(col.name);

            for (QueryRow row : this) {
                JArray jrow = arr.array();
                jrow.options.singleLine(true);
                for (int i = 0; i < row.values.length; i++)
                    jrow.add(row.getObj(i, null));
            }
        }

        if (!compact)
            for (QueryRow row : this) {
                JObject jobj = arr.object();
                for (QueryCell cell : row)
                    jobj.put(cell.column.name, cell.value);
            }

        return arr;
    }

    @Override
    protected TList<QueryRow> getRows() {
        return rows;
    }

}

package com.database;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.context.AppContext;
import com.json.Escape;
import com.lang.LDatabase;
import com.mlogger.Log;
import com.model.dao.core.DAORow;
import com.utils.TCurrency;
import com.utils.collections.TList;
import com.utils.text.StrWriter;
import java.sql.*;
import java.util.Date;
import java.util.*;
import java.util.Map.Entry;

public class QueryRow extends DAORow implements Iterable<QueryCell> {

    public static QueryRow getDummy(QueryRows parent) {
        return new QueryRow(new Object[0], parent);
    }

    QueryRows rows;
    public final Object[] values;
    public final Map<String, Object> extra = new HashMap<>();

    @Override
    public String toString() {

        LinkedList<QueryCell> cells = getValues();

        int max = 0;
        for (QueryCell cell : cells)
            if (cell.column.name.length() > max)
                max = cell.column.name.length();

        max += 2;
        StrWriter writer = new StrWriter();
        for (QueryCell cell : cells) {

            String s = cell.column.name + ":";

            while (s.length() < max)
                s += " ";

            s += Escape.escape(cell.value);

            if (!writer.isEmpty())
                writer.append("\n");
            writer.append(s);
        }

        return writer.toString();
    }

    public QueryRows getRows() {
        return rows;
    }

    public List<String> asList() {
        List<String> lst = new LinkedList<>();
        for (Object c : values)
            lst.add(c != null ? c.toString() : null);
        return lst;
    }

    public String[] asArray() {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++)
            result[i] = values[i] != null ? values[i].toString() : null;
        return result;
    }

    public Map<Object, Object> getMap(String column) throws SQLException {
        if (rows == null)
            return null;
        Map<Object, Object> map = rows.db.meta.processMap(getObj(column, null));
        return map != null ? map : new HashMap<>();
    }

    public Map<Object, Object> getMap(int column) throws SQLException {
        if (rows == null)
            return null;
        Map<Object, Object> map = rows.db.meta.processMap(getObj(column, null));
        return map != null ? map : new HashMap<>();
    }

    public Map<String, String> getMapString(String name) throws SQLException {
        Map<Object, Object> map = getMap(name);
        Map<String, String> result = new LinkedHashMap<>();
        try {
            for (Entry<Object, Object> en : map.entrySet()) {
                Object key = en.getKey();
                Object value = en.getValue();
                result.put(key != null ? key.toString() : null,
                        value != null ? value.toString() : null);
            }
        } catch (Exception e) {
            throw new SQLException(e);
        }

        return result;
    }

    QueryRow(Object[] cells, QueryRows rows) {
        super(rows);
        this.values = cells;
        this.rows = rows;
    }

    public int getColumnIdxDef(String columnName) {
        int result = -1;

        if (rows == null)
            return result;

        if (columnName != null) {
            if (columnName.startsWith("\"") && columnName.endsWith("\""))
                columnName = columnName.substring(1, columnName.length() - 1);

            for (String name : StrUtils.split(columnName))
                for (int i = 0; i < rows.columns.length; i++)
                    if (rows.columns[i].name.equalsIgnoreCase(name.trim())) {
                        result = i;
                        break;
                    }
            if (result != -1)
                return result;
        }

        if (AppContext.devMode)
            Log.warning("SQL", "Nie znaleziono kolumny " + Escape.escape(columnName) + "");

        return result;
    }

    public Integer getColumnIdx(String columnName) throws SQLException {
        int idx = getColumnIdxDef(columnName);
        if (idx < 0 && rows != null)
            throw new SQLException(LDatabase.COLUMN_NOT_FOUND.toString(columnName));

        return idx;
    }

    @Override
    protected TList<String> getDAONames() {
        return new TList<String>().addAll(rows.columns, col -> col.name);
    }

    @Override
    protected Object getDAOValue(String name) {
        return getObj(name, null);
    }

    @Override
    protected Object getDAOValue(int index) {
        return getObj(index, null);
    }

    public Object getObj(int column, Object def) {
        if (column >= 0 && column < values.length)
            return values[column] != null ? values[column] : def;
        return def;
    }

    public Object getObj(String column, Object def) {
        int col = getColumnIdxDef(column);
        if (col >= 0 && col < values.length)
            return values[col] != null ? values[col] : def;
        return def;
    }

    /**
     * Wartość musi istnieć, nie może być null-em
     */
    public Object getObj(String column) throws SQLException {
        if (rows == null)
            return null;

        int col = getColumnIdx(column);
        if (col < 0)
            throw new SQLException(LDatabase.COLUMN_NOT_FOUND.toString(column));
        if (values[col] == null)
            throw new SQLException(LDatabase.RECORD_CONTAINS_NULL.toString(column));

        return values[col];
    }

    /**
     * Wartość musi istnieć, nie może być null-em
     */
    public Object getObj(int column) throws SQLException {
        if (rows == null)
            return null;

        if (column >= 0 && column < values.length) {

            if (values[column] == null)
                throw new SQLException(
                        LDatabase.RECORD_CONTAINS_NULL.toString(column)
                        + " (" + rows.columns[column].name + ")");

            return values[column];
        }
        throw new SQLException(LDatabase.COLUMN_NOT_FOUND.toString(column));
    }

    public String getStr(int column, String def) {
        Object obj = getObj(column, def);
        return obj != null ? obj.toString() : def;
    }

    public String getStr(String column, String def) {
        Object obj = getObj(column, def);
        return obj != null ? obj.toString() : def;
    }

    public String getStr(int column) throws SQLException {
        return objToString(getObj(column));
    }

    private String objToString(Object obj) {
        if (obj == null)
            return null;

        //  if (obj instanceof Number)
        //       return Utils.formatValue((Number) obj);
        return obj.toString();
    }

    public String getStr(String column) throws SQLException {
        return objToString(getObj(column));
    }

    /**
     * Zwraca wartość numeryczną sformatowaną do postaci z separatoramim np 232
     * 323.567
     *
     * @param column
     * @param def
     * @return
     * @throws SQLException
     */
    public String getNumber(String column, String def) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Number)
            return Utils.formatValue((Number) obj);
        return def;
    }

    public Integer getInt(String column, Integer def) {
        return getInt(getColumnIdxDef(column), def);
    }

    public Integer getInt(int column, Integer def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Integer)
                return (Integer) obj;
            return Integer.parseInt(obj.toString());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public char getChar(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Character)
            return (Character) obj;
        return obj.toString().charAt(0);
    }

    public Character getChar(String column, Character def) throws SQLException {
        try {
            Object obj = getObj(column);
            if (obj instanceof Character)
                return (Character) obj;

            return obj.toString().charAt(0);
        } catch (Throwable e) {
            return def;
        }
    }

    public int getInt(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Integer)
            return (Integer) obj;
        return Integer.parseInt(obj.toString());
    }

    public int getInt(int column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Integer)
            return (Integer) obj;
        return Integer.parseInt(obj.toString());
    }

    public Long getLong(String column, Long def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Long)
                return (Long) obj;
            return Long.parseLong(obj.toString());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public long getLong(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Float)
            return (Long) obj;
        return Long.parseLong(obj.toString());
    }

    public long getLong(int column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Float)
            return (Long) obj;
        return Long.parseLong(obj.toString());
    }

    public Long getLong(int column, Long def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Long)
                return (Long) obj;
            return Long.parseLong(obj.toString());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public Float getFloat(String column, Float def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Float)
                return (Float) obj;
            return Float.parseFloat(obj.toString());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public float getFloat(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Float)
            return (Float) obj;
        return Float.parseFloat(obj.toString());
    }

    public Double getDouble(String column, Double def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Double)
                return (Double) obj;
            return Double.parseDouble(obj.toString());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public TCurrency getCurrency(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Double)
            return new TCurrency((Double) obj);
        return TCurrency.parse(obj.toString());
    }

    public String getCurrencyFrmt(String column, TCurrency def) {
        TCurrency curr = getCurrency(column, def);
        return curr != null ? curr.format() : null;
    }

    public TCurrency getCurrency(String column, double def) {
        return getCurrency(column, new TCurrency(0));
    }

    public TCurrency getCurrency(String column, TCurrency def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Double)
                return new TCurrency((Double) obj);
            return TCurrency.parse(obj.toString());
        } catch (NumberFormatException e) {
            return def;
        }
    }

    public double getDouble(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Double)
            return (Double) obj;
        return Double.parseDouble(obj.toString());
    }

    public Boolean getBool(String column, Boolean def) {
        Object obj = getObj(column, def);
        if (obj == null)
            return def;
        if (obj instanceof Boolean)
            return (Boolean) obj;
        return Utils.strBool(obj.toString(), def);
    }

    public Boolean getBool(int column, Boolean def) {
        Object obj = getObj(column, def);
        if (obj == null)
            return def;
        if (obj instanceof Boolean)
            return (Boolean) obj;
        return Utils.strBool(obj.toString(), def);
    }

    public boolean getBool(String column) throws SQLException {
        return getBool(getColumnIdx(column));
    }

    public boolean getBool(int column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Boolean)
            return (Boolean) obj;
        Boolean bool = Utils.strBool(obj.toString(), null);
        if (bool == null)
            throw new SQLException(LDatabase.INVALID_BOOL_VALUE.toString());
        return bool;
    }

    public String getBoolStr(String column) throws SQLException {
        return Utils.boolToStr(QueryRow.this.getBool(column));
    }

    public String getBoolStr(String column, Boolean def) {
        return Utils.boolToStr(getBool(column, def));
    }

    public TDate getDate(String column, TDate def) {
        try {
            Object obj = getObj(column, def);
            if (obj == null)
                return def;
            if (obj instanceof Date)
                return new TDate((Date) obj);
            return new TDate(obj.toString());
        } catch (Exception e) {
            return def;
        }
    }

    public TDate getDate(String column) throws SQLException {
        Object obj = getObj(column);
        if (obj instanceof Date)
            return new TDate((Date) obj);
        try {
            return new TDate(obj.toString());
        } catch (Exception ex) {
            throw new SQLException(ex);
        }
    }

    public LinkedList<Integer> getArrayInt(String column, boolean allowNulls) throws SQLException {

        LinkedList<Integer> lst = new LinkedList<>();

        for (Object o : getArrayObj(column)) {

            if (o == null && !allowNulls)
                continue;

            if (o == null) {
                lst.add(null);
                continue;
            }

            if (o instanceof Number) {
                lst.add(((Number) o).intValue());
                continue;
            }

            Integer ii = Utils.strInt(o.toString(), null);
            if (ii == null && !allowNulls)
                continue;
            lst.add(ii);
        }
        return lst;
    }

    public Strings getArray(String column) throws SQLException {
        return getArray(getColumnIdxDef(column), false);
    }

    public Strings getArray(String column, boolean allowNulls) throws SQLException {
        return getArray(getColumnIdxDef(column), allowNulls);
    }

    public Collection<Object> getArrayObj(int column) throws SQLException {
        return rows != null ? rows.db.meta.processArray(getObj(column, null)) : null;
    }

    public Collection<Object> getArrayObj(String column) throws SQLException {
        return rows != null ? rows.db.meta.processArray(getObj(column, null)) : null;
    }

    public Strings getArray(int column, boolean allowNulls) throws SQLException {
        Strings list = new Strings().allowNulls(allowNulls);
        if (column >= 0)
            for (Object o : QueryRow.this.getArrayObj(column))
                list.add(o);
        return list;
    }

    public String getDateStr(String column, String def, boolean includeMilliseconds) {
        return getDateStr(getColumnIdxDef(column), def, includeMilliseconds);
    }

    public String getDateStr(int column, String def, boolean includeMilliseconds) {
        String d = getStr(column, null);
        if (d == null)
            return def;

        String ms = "";
        if (d.indexOf(".") > 0) {
            ms = d.substring(d.lastIndexOf(".") + 1, d.length());
            d = d.substring(0, d.lastIndexOf("."));

            if (!includeMilliseconds)
                return d;

            while (ms.length() < 3)
                ms = "0" + ms;

            if (ms.length() > 3)
                ms = ms.substring(0, 3);

            return d + "." + ms;

        }
        return d;
    }

    public boolean has(String name) {
        if (rows != null)
            for (QueryColumn col : rows.columns)
                if (col.name.equalsIgnoreCase(name))
                    return true;
        return false;
    }

    public LinkedList<QueryCell> getValues() {
        LinkedList<QueryCell> cells = new LinkedList<>();
        for (QueryColumn col : rows.columns)
            cells.add(new QueryCell(col, this));
        return cells;
    }

    /*
     public void writeToObject(Object object) throws IllegalArgumentException, IllegalAccessException {
     if (object == null)
     return;

     for (Field f : object.getClass().getFields()) {
     Col col = f.getAnnotation(Col.class);
     if (col == null)
     continue;

     String name = col.name().isEmpty() ? f.getName() : col.name();

     int idx = getColumnIdxDef(name);
     if (idx < 0)
     return;

     Object src = cells[idx];

     // if (src == null && !col.flags().)
     // continue;
     Object dst = f.get(object);

     if (src.getClass() == dst.getClass()) {
     f.set(object, dst);
     continue;
     }

     if (src instanceof Number && dst instanceof Boolean) {
     f.set(object, dst);
     continue;
     }

     if (f.getType().isArray() || f.getType() == Collection.class
     || f.getType() == Set.class || f.getType() == List.class) {

     }

     }

     }
     */
    @Override
    public Iterator<QueryCell> iterator() {
        return getValues().iterator();
    }

}

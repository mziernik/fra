package com.database;

import com.utils.collections.Strings;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.*;

public class QueryColumn {

    public final QueryRows row;
    public final String name;
    public final String type;
    public final int index;
    public final String table;
    public final String schema;

    QueryColumn(ResultSetMetaData meta, int idx, QueryRows rows) throws SQLException {
        this.type = meta.getColumnTypeName(idx + 1);
        this.name = meta.getColumnLabel(idx + 1);
        this.row = rows;
        this.index = idx;
        this.table = meta.getTableName(idx + 1);
        this.schema = meta.getSchemaName(idx + 1);
    }

    public int getNonEmptyCellsCount() {
        int cnt = 0;
        for (QueryRow qr : row)
            if (qr.values[index] != null)
                ++cnt;
        return cnt;
    }

    public List<Object> getData() {
        List<Object> list = new LinkedList<>();
        for (QueryRow qr : row)
            list.add(qr.values[index]);
        return list;
    }

    public List<Object> getContent() {
        LinkedList<Object> lst = new LinkedList<>();
        for (QueryRow qr : row)
            lst.add(qr.values[index]);
        return lst;
    }

    public Strings getContentStr() {
        Strings lst = new Strings();
        for (QueryRow qr : row)
            lst.add(qr.values[index]);
        return lst;
    }

    public List<Number> getContentNumber() {
        LinkedList<Number> lst = new LinkedList<>();
        for (QueryRow qr : row)
            lst.add(qr.values[index] instanceof Number ? (Number) qr.values[index] : null);
        return lst;
    }

}

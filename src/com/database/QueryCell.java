package com.database;

/**
 * @author Miłosz Ziernik
 * @date 30 grudnia 2015
 * @encoding UTF-8
 */
public class QueryCell {

    public final QueryColumn column;
    public final QueryRow row;
    public final Object value;

    public QueryCell(QueryColumn column, QueryRow row) {
        this(column, row, row != null ? row.values[column.index] : null);
    }

    QueryCell(QueryColumn column, QueryRow row, Object value) {
        this.value = value;
        this.row = row;
        this.column = column;
    }

    QueryCell(QueryColumn column, Object value) {
        this(column, null, value);
    }

}

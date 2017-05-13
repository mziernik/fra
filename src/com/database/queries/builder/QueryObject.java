package com.database.queries.builder;

import com.database.Database;
import com.database.SqlCondition;
import com.utils.*;
import com.utils.reflections.TClass;
import com.utils.text.StrWriter;
import java.io.StringWriter;
import java.util.*;

public class QueryObject {

    private Boolean escape;
    private Boolean quote;
    private String cast;
    private final Database db;
    private QueryColumn column;
    Object value;
    String name;
    private Boolean array;
    private Boolean isCollection;

    public String name() {
        return name;
    }

    public Object getValue() {
        return value;
    }

    public Boolean escape() {
        return escape;
    }

    public Boolean quote() {
        return quote;
    }

    public String cast() {
        return cast;
    }

    public Boolean array() {
        return array;
    }

    public QueryObject(Database db, Object value) {
        this.db = db;
        this.value = value;
    }

    public QueryObject name(String name) {
        return name(new QueryColumn(Objects.requireNonNull(name, "column_name")));
    }

    public QueryObject name(QueryColumn column) {
        this.column = Objects.requireNonNull(column);
        this.name = Objects.requireNonNull(column.name);
        this.cast = column.cast;
        return this;
    }

    public QueryColumn column() {
        return column;
    }

    public QueryObject value(Object value) {
        this.value = value;
        return this;
    }

    /**
     * Czy kolekcję traktować jako tablicę
     *
     * @param array
     * @return
     */
    public QueryObject array(Boolean array) {
        this.array = array;
        return this;
    }

    public QueryObject cast(String cast) {
        this.cast = cast;
        return this;
    }

    public QueryObject escape(Boolean escape) {
        this.escape = escape;
        return this;
    }

    public QueryObject quote(Boolean quote) {
        this.quote = quote;
        return this;
    }

    @Override
    public String toString() {
        return getEscapedValue();
    }

    public StringWriter getValue(StrWriter qry) {
        return db.escape(qry, this);
    }

    public String getEscapedValue() {
        return getValue(new StrWriter()).toString();
    }

    public boolean needEscape() {
        return !(value == null
                || value == Undefined.TYPE
                || value instanceof IUnquoted
                || value instanceof Boolean
                || value instanceof Number
                || value instanceof SqlCondition);
    }

    public void setIsCollection(Boolean isCollection) {
        this.isCollection = isCollection;
    }

    public boolean isCollection() {
        return isCollection != null
                ? isCollection
                : (value != null && value.getClass().isArray()) || (value instanceof Iterable);
    }

    public LinkedList<Object> asCollection() {

        LinkedList<Object> list = new LinkedList<>();

        if (value != null && value.getClass().isArray()) {
            for (int i = 0; i < java.lang.reflect.Array.getLength(value); i++)
                list.add(java.lang.reflect.Array.get(value, i));
            return list;
        }

        if (value instanceof Iterable) {
            Iterator<Object> it = ((Iterable) value).iterator();
            while (it.hasNext())
                list.add(it.next());
            return list;
        }

        list.add(value);
        return list;
    }

}

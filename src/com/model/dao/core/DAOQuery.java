package com.model.dao.core;

import com.model.repository.intf.CRUDE;
import com.utils.collections.Pair;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import java.util.LinkedHashMap;
import java.util.Map;

public class DAOQuery {

    public class DaoParam {

        public DataType type;
        public Object value;
        public String cast;

        public DaoParam(DataType type, Object value, String cast) {
            this.type = type;
            this.value = value;
            this.cast = cast;
        }

    }

    public final DAO dao;
    public final CRUDE crude;
    public final Object context;

    public String source;
    public String primaryKeyName;
    public Object primaryKeyValue;

    /**
     * nazwy plików, tabel
     */
    public final TList<String> field = new TList<String>().notNull();
    /**
     * Parametry, argumenty dla insert-a, update
     */
    public final Map<String, DaoParam> params = new LinkedHashMap<>();

    public final TList<Pair<String, Boolean>> order = new TList<Pair<String, Boolean>>().notNull();

    public String getQuery() {
        return null;
    }

    public DAOQuery(Object context, DAO dao, CRUDE crude) {
        this.dao = dao;
        this.crude = crude;
        this.context = context;
    }

    /**
     * Nazwa tabeli
     */
    public DAOQuery source(String name) {
        this.source = name;
        return this;
    }

    /**
     * Nazwa kolumny
     */
    public DAOQuery field(String name) {
        field.add(name);
        return this;
    }

    /**
     * Kolumna, po której sortujemy
     */
    public DAOQuery order(String name, boolean desc) {
        return this;
    }

    /**
     * Where
     */
    public DAOQuery condition(String name, String type, Object value) {
        return this;
    }

    /**
     * Kolumna, po której sortujemy
     */
    public DAOQuery param(String name, DataType type, Object value, String cast) {
        params.put(name, new DaoParam(type, value, cast));
        return this;
    }
}

package com.model.dao.core;

import com.model.repository.intf.CRUDE;
import com.utils.collections.Pair;
import com.utils.collections.TList;

public class DAOQuery {

    public final DAO dao;
    public final CRUDE crude;
    public final Object context;

    public final TList<String> source = new TList<String>().notNull();
    public final TList<String> field = new TList<String>().notNull();
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
        source.add(name);
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

}

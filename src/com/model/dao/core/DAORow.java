package com.model.dao.core;

import com.utils.Utils;
import com.utils.collections.TList;

public abstract class DAORow {

    private final DAO dao;
    private final DAORows rows;
    private TList<String> names;

    public DAORow(DAORows rows) {
        this.dao = rows.dao;
        this.rows = rows;
    }

    protected abstract TList<String> getDAONames();

    protected abstract Object getDAOValue(String name);

    protected abstract Object getDAOValue(int index);

    public TList<String> getNames() {
        if (names == null)
            names = getDAONames();
        return names;
    }

    public boolean contains(String name) {
        return getNames().contains(name);
    }

    public boolean isEmpty() {
        return getNames().isEmpty();
    }

    public Object getValue(String name, Object defaultValue) {
        return Utils.coalesce(getDAOValue(name), defaultValue);
    }
}

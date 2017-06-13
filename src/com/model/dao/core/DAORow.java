package com.model.dao.core;

import com.utils.Utils;

public abstract class DAORow {
    
    private final DAO dao;
    private final DAORows rows;
    
    public DAORow(DAORows rows) {
        this.dao = rows.dao;
        this.rows = rows;
    }
    
    protected abstract Object getDAOValue(String name);
    
    protected abstract Object getDAOValue(int index);
    
    public Object getValue(String name, Object defaultValue) {
        return Utils.coalesce(getDAOValue(name), defaultValue);
    }
}

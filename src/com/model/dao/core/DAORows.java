package com.model.dao.core;

import com.utils.collections.TList;
import java.util.Iterator;

public abstract class DAORows<ROW extends DAORow> implements Iterable<ROW> {

    final DAO dao;
    final DAOQuery query;

    public DAORows(DAOQuery query) {
        this.query = query;
        this.dao = query.dao;
    }

    protected abstract TList<ROW> getRows();

    @Override
    public Iterator<ROW> iterator() {
        return getRows().iterator();
    }

}

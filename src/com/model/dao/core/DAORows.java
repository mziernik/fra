package com.model.dao.core;

import com.model.repository.intf.CRUDE;
import com.utils.collections.TList;
import java.util.Iterator;

public abstract class DAORows<ROW extends DAORow> implements Iterable<ROW> {

    public DAO dao;
    public DAOQuery query;
    public Object context;
    public CRUDE crude;

    private TList<ROW> rows;

    public DAORows(DAOQuery query) {
        this.query = query;
        this.dao = query != null ? query.dao : null;
        this.context = query != null ? query.context : null;
        this.crude = query != null ? query.crude : null;
    }

    protected abstract TList<ROW> getRows();

    private TList<ROW> _getRows() {
        if (rows == null)
            rows = getRows();
        return rows;
    }

    @Override
    public Iterator<ROW> iterator() {
        return _getRows().iterator();
    }

    public int size() {
        return _getRows().size();
    }

    public boolean isEmpty() {
        return _getRows().isEmpty();
    }

    public ROW first() {
        return _getRows().first();
    }

}

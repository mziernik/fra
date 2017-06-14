package com.model.dao.core;

import com.model.repository.intf.CRUDE;
import com.utils.collections.TList;
import java.util.Iterator;

public abstract class DAORows<ROW extends DAORow> implements Iterable<ROW> {

    public DAO dao;
    public DAOQuery query;
    public Object context;
    public CRUDE crude;

    public DAORows(DAOQuery query) {
        this.query = query;
        this.dao = query != null ? query.dao : null;
        this.context = query != null ? query.context : null;
        this.crude = query != null ? query.crude : null;
    }

    protected abstract TList<ROW> getRows();

    @Override
    public Iterator<ROW> iterator() {
        return getRows().iterator();
    }

}

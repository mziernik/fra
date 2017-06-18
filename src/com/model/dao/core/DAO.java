package com.model.dao.core;

import com.utils.collections.TList;

public interface DAO<ROWS extends DAORows<?>> {

    public TList<ROWS> process(TList<? extends DAOQuery> queries) throws Exception;

    default ROWS process(DAOQuery query) throws Exception {
        return process(new TList<>(query)).first();
    }

}

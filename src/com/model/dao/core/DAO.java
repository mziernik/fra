package com.model.dao.core;

import com.utils.collections.TList;

public interface DAO<ROWS extends DAORows<?>> {

    public TList<ROWS> process(TList<? extends DAOQuery> queries) throws Exception;

    default ROWS process(DAOQuery query) throws Exception {
        return process(new TList<>(query)).first();
    }

    /**
     * Czy transakcje są obsługiwane
     * @return 
     */
    default boolean isTransactional() {
        return false;
    }

    default boolean inTransaction() {
        throw new UnsupportedOperationException();
    }

    default void beginTransaction() {
        throw new UnsupportedOperationException();
    }

    default void commitTransaction() {
        throw new UnsupportedOperationException();
    }

    default void rollbackTransaction() {
        throw new UnsupportedOperationException();
    }
}

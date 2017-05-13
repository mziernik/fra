package com.database.async;

import com.database.queries.MultipleQuery;

@FunctionalInterface
public interface AsyncQueryProvider {

    public void build(MultipleQuery qry) throws Exception;
}

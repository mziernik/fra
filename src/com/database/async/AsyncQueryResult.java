package com.database.async;

import com.database.QueryRows;

@FunctionalInterface
public interface AsyncQueryResult {

    public void onResult(QueryRows rows) throws Exception;

}

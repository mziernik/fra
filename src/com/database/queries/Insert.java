package com.database.queries;

import com.database.Database;

public class Insert extends InsertOrUpdate {

    public Insert(Database db, String table) {
        super(db, table, null);
    }

    @Override
    public String buildQuery() {
        return buildInsert();
    }
}

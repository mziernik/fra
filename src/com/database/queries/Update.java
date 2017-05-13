package com.database.queries;

import com.database.Database;

public class Update extends InsertOrUpdate {

    public Update(Database db, String table, String where) {
        super(db, table, where);
    }

    @Override
    public String buildQuery() {
        return buildUpdate();
    }
}

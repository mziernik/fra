package com.database.queries;

import com.database.Database;

public class InsertOrReplace extends InsertOrUpdate {

    public InsertOrReplace(Database db, String table) {
        super(db, table, null);
        orReplace = true;
    }

    @Override
    public String buildQuery() {
        return buildInsert();
    }
}

package com.database.orm;

import com.database.QueryRow;
import com.database.QueryRows;
import java.sql.SQLException;
import java.util.Iterator;

public class DbRows {

    public final QueryRows rows;
    public final DbColumn<?, ?>[] columns;
    private final Iterator<QueryRow> itr;

    public DbRows(QueryRows rows, DbColumn<?, ?>[] columns) {
        this.rows = rows;
        this.columns = columns;
        this.itr = rows.iterator();
    }

    public boolean next() throws SQLException {

        if (!itr.hasNext())
            return false;

        QueryRow row = itr.next();

        for (DbColumn col : columns)
            col.value = row.getObj(col.name);

        return true;
    }
}

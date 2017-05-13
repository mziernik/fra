package com.database.queries;

import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.database.QueryRows;
import com.database.queries.builder.QueryObject;
import com.exceptions.SQLError;
import java.sql.*;

public class StoredProc extends QueryBuilder<StoredProc> {

    private final String procName;

    public StoredProc(Database db, String procName) {
        super(db);
        this.procName = procName;
    }

    @Override
    public QueryRows execute() throws SQLException {
        return db.execute(toString());
    }

    /**
     * Dodaj parametr, nazwa jako komentarz
     *
     * @param name
     * @param value
     * @return
     * @throws SQLError
     */
    @Override
    public StoredProc arg(String name, Object value) {
        options.addNameComments(true);
        params.add(new QueryObject(db, value).name(name));
        return this;

    }

    public StoredProc add(Object value) {
        params.add(new QueryObject(db, value));
        return this;
    }

    @SuppressWarnings("unchecked")
    public StoredProc add(Object value, Boolean escape, String cast) {
        params.add(new QueryObject(db, value).escape(escape).cast(cast));
        return this;
    }

    @Override
    public String buildQuery() {
        return "SELECT * FROM " + procName + "("
                + (params.isEmpty() ? "" : "\n" + getValues() + "\n") + ")";
    }
}

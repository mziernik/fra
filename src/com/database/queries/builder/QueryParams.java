package com.database.queries.builder;

import com.database.Database;
import com.database.queries.builder.QueryBuilder;

/**
 * Klasa formatuje i escepuje parametry zapytania
 *
 * @author milosz
 */
public class QueryParams {

    private final Database db;
    private final QueryBuilder builder;
    public final QueryOptions options;

    public QueryParams(Database db) {
        this.db = db;
        this.builder = new QueryBuilder(db) {

            @Override
            protected String buildQuery() {
                return null;
            }
        };
        options = builder.options;
        options.singleLine(true)
                .intent(" ");
    }

    public QueryParams add(Object value, Boolean escape, String cast) {
        builder.params.add(new QueryObject(db, value).escape(escape).cast(cast));
        return this;
    }

    public QueryParams add(Object value) {
        return add(value, null, null);
    }

    public QueryParams addAll(Object... values) {
        if (values != null)
            for (Object o : values)
                add(o, null, null);
        return this;
    }

    @Override
    public String toString() {
        return builder.getValues().trim();
    }

}

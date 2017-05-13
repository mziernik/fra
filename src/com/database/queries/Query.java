package com.database.queries;

import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.exceptions.SQLError;
import com.database.queries.builder.QueryObject;

/**
 * W zapytaniu podmieniane są zmienne w formie :name na wartości Miłosz Ziernik
 * 2014/06/19
 */
public class Query extends QueryBuilder<Query> {

    private final String query;
    private final Object[] qParams;

    public Query(Database db, String query, Object... params) {
        super(db);
        this.query = query;
        this.qParams = params;
    }

    @Override
    public Query arg(String name, Object value) {
        return super.arg(name, value);
    }

    @Override
    public boolean isEmpty() {
        return query == null || query.isEmpty();
    }

    @Override
    public String buildQuery() throws SQLError {
        String q = query;
        if (!isEmpty())
            for (QueryObject qp : this)
                if (qp != null && qp.name() != null && !qp.name().isEmpty())
                    q = q.replace(":" + qp.name(), qp.getEscapedValue());

        return db.processParams(q, qParams);
    }
}

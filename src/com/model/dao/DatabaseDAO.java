package com.model.dao;

import com.database.Database;
import com.database.QueryRows;
import com.database.queries.MultipleQuery;
import com.database.queries.Query;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.servlet.Handlers;
import com.utils.collections.TList;
import com.utils.text.StrWriter;

public class DatabaseDAO implements DAO<QueryRows> {

    Database db;

    public DatabaseDAO() {
        this(null);
    }

    public DatabaseDAO(Database db) {
        if (db == null && this instanceof Database)
            db = (Database) this;

        if (db == null)
            db = Handlers.database.getInstance().getDatabase();

        this.db = db;
    }

    @Override
    public TList<QueryRows> process(TList<? extends DAOQuery> queries) throws Exception {

        TList<QueryRows> result = new TList<>();
        MultipleQuery mqry = db.multipleQuery();
        DAOQuery query = null;
        for (DAOQuery q : queries)
            mqry.add(getQuery(query = q));

        if (query == null)
            return result;

        QueryRows rows = mqry.execute();
        rows.context = query.context;
        rows.dao = query.dao;
        rows.crude = query.crude;
        result.add(rows);
        return result;

    }

    private Query getQuery(DAOQuery query) throws Exception {

        switch (query.crude) {
            case READ:
                return select(db, query);
            default:
                throw new UnsupportedOperationException();
        }

    }

    public Query select(Database db, DAOQuery query) throws Exception {

        StrWriter sb = new StrWriter();
        for (String name : query.field)
            sb.append(sb.length() == 0 ? "SELECT " : ", ")
                    .append(name);

        sb.append("\nFROM ").join(query.source, ", ", null);

        if (!query.order.isEmpty())
            sb.append("\nORDER BY ")
                    .join(query.order, ", ", (p) -> p.first + (p.second ? " DESC" : " ASC"));

        return db.query(sb.toString());
    }

}

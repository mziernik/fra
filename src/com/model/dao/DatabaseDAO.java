package com.model.dao;

import com.database.Database;
import com.database.QueryRow;
import com.database.QueryRows;
import com.database.queries.MultipleQuery;
import com.database.queries.Query;
import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.intf.runnable.RunnableEx2;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.servlet.Handlers;
import com.utils.collections.TList;
import com.utils.text.StrWriter;
import java.sql.SQLException;
import java.util.Objects;

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

        queries = new TList<>(queries); // kopia
        boolean markers = queries.size() > 1;

        TList<QueryRows> result = new TList<>();
        MultipleQuery mqry = db.multipleQuery();
        DAOQuery query = null;
        for (DAOQuery q : queries) {
            if (markers)
                mqry.query("SELECT ? AS class_name, ? AS hash_code",
                        q.context.getClass().getName(), q.hashCode());

            mqry.add(getQuery(query = q));
        }
        if (query == null)
            return result;

        QueryRows rows = mqry.execute();

        if (!markers) {
            rows.context = query.context;
            rows.dao = query.dao;
            rows.crude = query.crude;
            result.add(rows);
            return result;
        }

        while (rows != null) {
            if (rows.size() != 1)
                throw new SQLException("Unexpected results count (" + rows.size() + ")");

            QueryRow row = rows.firstD();

            if (rows.size() != 1 || rows.columns.length != 2
                    || !"class_name".equals(rows.columns[0].name)
                    || !"hash_code".equals(rows.columns[1].name))
                throw new ServiceException("Unexpected data");

            rows = rows.nextResults();

            String cn = row.getStr("class_name");
            int hc = row.getInt("hash_code");

            DAOQuery rec = queries.removeFirst();

            if (!rec.context.getClass().getName().equals(cn) || rec.hashCode() != hc)
                throw new SQLException("Unexpected onject");

            QueryRows qr = Objects.requireNonNull(rows, "No results");
            qr.context = rec.context;
            qr.dao = rec.dao;
            qr.crude = rec.crude;
            result.add(qr);
            rows = rows.nextResults();

        }
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

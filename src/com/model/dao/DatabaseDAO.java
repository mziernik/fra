package com.model.dao;

import com.database.Database;
import com.database.QueryRow;
import com.database.QueryRows;
import com.database.queries.InsertOrUpdate;
import com.database.queries.MultipleQuery;
import com.database.queries.Query;
import com.database.queries.builder.QueryBuilder;
import com.exceptions.ServiceException;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAOQuery.DaoParam;
import com.servlet.Handlers;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import com.utils.text.StrWriter;
import java.sql.SQLException;
import java.util.Date;
import java.util.Map.Entry;
import java.util.Objects;

public class DatabaseDAO implements DAO<QueryRows> {

    private final Database db;

    public static DatabaseDAO create() {
        return Handlers.database.getInstance().getDatabase();
    }

    protected DatabaseDAO() {
        db = (Database) this;
    }

    @Override
    public boolean isTransactional() {
        return true;
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

    private QueryBuilder getQuery(DAOQuery query) throws Exception {
        switch (query.crude) {
            case READ:
                return select(db, query);
            case CREATE:
                return insertOrUpdate(db, query, true);
            case UPDATE:
                return insertOrUpdate(db, query, false);
            case DELETE:
                return delete(db, query);
            default:
                throw new UnsupportedOperationException();
        }
    }

    public InsertOrUpdate insertOrUpdate(Database db, DAOQuery query, boolean insert) throws Exception {
        InsertOrUpdate ins = insert
                ? db.insert(query.source)
                : db.update(query.source, query.primaryKeyName + " = ?", query.primaryKeyValue);

        for (String s : query.field)
            ins.addReturningColumn(s);

        for (Entry<String, DaoParam> en : query.params.entrySet()) {
            DaoParam p = en.getValue();

            Object value = p.value;
            if (p.type == DataType.TIMESTAMP && value instanceof Number)
                value = new Date(((Number) value).longValue());

            ins.arg_(en.getKey(), value).cast(p.cast);
        }

        return ins;
    }

    public Query delete(Database db, DAOQuery query) throws Exception {
        return db.query("DELETE FROM " + query.source + " WHERE "
                + query.primaryKeyName + " = ?", query.primaryKeyValue);
    }

    public Query select(Database db, DAOQuery query) throws Exception {

        StrWriter sb = new StrWriter();
        for (String name : query.field)
            sb.append(sb.length() == 0 ? "SELECT " : ", ")
                    .append(name);

        sb.add("\nFROM ", query.source);

        if (!query.order.isEmpty())
            sb.append("\nORDER BY ")
                    .join(query.order, ", ", (p) -> p.first + (p.second ? " DESC" : " ASC"));

        return db.query(sb.toString());
    }

}

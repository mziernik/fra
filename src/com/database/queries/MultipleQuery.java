package com.database.queries;

import com.exceptions.SQLError;
import com.database.queries.builder.QueryBuilder;
import com.database.*;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import java.sql.SQLException;

public class MultipleQuery extends QueryBuilder<MultipleQuery> {

    public QueryExecutor executor;
    public final TList<QueryBuilder> list = new TList<>();

    public MultipleQuery(Database db) {
        super(db);
    }

    public MultipleQuery add(QueryBuilder builder) {
        list.add(builder);
        return this;
    }

    public Query query(String query, Object... params) {
        Query qry = new Query(db, query, params);
        qry.executor = this::exec;
        list.add(qry);
        return qry;
    }

    public Insert insert(String table) {
        Insert ins = new Insert(db, table);
        list.add(ins);
        return ins;
    }

    public InsertMultiple insertMultiple(String table, String... columns) throws SQLError {
        InsertMultiple ins = new InsertMultiple(db, table, columns);
        list.add(ins);
        return ins;
    }

    public UpdateMultiple updateMultiple(String table, String keyColumn, String... columns) throws SQLError {
        UpdateMultiple upd = new UpdateMultiple(db, table, keyColumn, columns);
        list.add(upd);
        return upd;
    }

    public Merge merge(String table, String keyColumn) {
        Merge merge = new Merge(db, table, keyColumn);
        list.add(merge);
        return merge;
    }

    public Upsert upsert(String table, String... keyColumns) {
        Upsert upsert = new Upsert(db, table, keyColumns);
        list.add(upsert);
        return upsert;
    }

    public Update update(String table, String where, Object... params) throws SQLError {
        Update update = new Update(db, table, db.processParams(where, params));
        list.add(update);
        return update;
    }

    public InsertOrUpdate insertOrUpdate(String table, String where, Object... params) throws SQLError {
        InsertOrUpdate update = new InsertOrUpdate(db, table, db.processParams(where, params));
        list.add(update);
        return update;
    }

    public StoredProc storedProc(String procName) {
        StoredProc proc = new StoredProc(db, procName);
        list.add(proc);
        return proc;
    }

    @Override
    protected String buildQuery() throws SQLException {
        Strings lst = new Strings().nonEmpty(true);
        for (QueryBuilder qry : list)
            if (!qry.isEmpty())
                lst.add(qry.toString());
        return lst.toString("\n;\n");
    }

    @Override
    public QueryRows execute() throws SQLException {
        return execute(!db.isTransaction());
    }

    public QueryRows execute(boolean transaction) throws SQLException {
        if (!transaction)
            return db.execute(toString());

        db.beginTransaction();
        try {
            QueryRows rows = db.execute(toString());
            db.commitTransaction();
            return rows;
        } catch (Throwable e) {
            db.rollbackTransaction();
            throw e;
        }

    }

    private QueryRows exec(QueryBuilder qry) throws SQLException {
        return execute();
    }

    @Override
    public boolean isEmpty() {
        return list.isEmpty();
    }

}

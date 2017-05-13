package com.database.orm;

import com.database.Database;
import com.database.QueryRow;
import com.database.QueryRows;
import com.database.SqlCondition;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.queries.Insert;
import com.database.queries.Merge;
import com.database.queries.Update;
import com.database.queries.builder.QueryBuilder;
import com.exceptions.SQLError;
import com.lang.LDatabase;
import com.utils.collections.Strings;
import java.sql.SQLException;
import java.util.*;

public abstract class DbTable<Schema extends DbSchema, Self extends DbTable>
        extends DbTableOrView<Schema, Self> {

    protected DbPrimaryKey primaryKey;
    protected List<DbIndex> indexes = new LinkedList<>();
    protected List<DbForeignKey> foreignKeys = new LinkedList<>();

    public DbTable(Class<Schema> cls, String schemaName, String name, String type) {
        super(cls, schemaName, name, type);
    }

    public DbRows select(DbColumn<Self, ?>... columns) throws SQLException {

        Strings cols = new Strings();
        for (DbColumn<Self, ?> col : columns)
            cols.add(col.name);

        QueryRows rows = db().execute("SELECT "
                + cols.toString(", ") + " FROM " + super.name);

        return new DbRows(rows, columns);

    }

    public QueryRows insert() throws SQLException {
        return insert(null);
    }

    public Insert insertQuery(Database db) throws SQLError {
        if (db == null)
            db = db();
        Insert ins = db.insert(fullName);

        for (DbColumn col : columns)
            if (col.modified)
                ins.arg(col.name, col.value);

        ins.addReturningColumn("*");
        return ins;
    }

    public QueryRows insert(Database db) throws SQLException {
        QueryBuilder ins = insertQuery(db);
        QueryRows rows = ins.execute();
        QueryRow row = rows.first();
        if (row != null)
            deserialize(row);
        return rows;
    }

    public QueryRows update(DbColumn<Self, ?>... keys) throws SQLException {
        return update(null, keys);
    }

    public Update updateQuery(Database db, DbColumn<Self, ?>... keys) throws SQLException {
        if (db == null)
            db = db();

        SqlCondition cond = new SqlCondition(true);

        if (keys == null || keys.length == 0)
            keys = primaryKey.columns;

        for (DbColumn<Self, ?> col : keys)
            if (col.modified)
                cond.add(col.name + " = " + db.escape(col.value));

        if (cond.isEmpty()) {
            Strings strs = new Strings();
            for (DbColumn<?, ?> c : primaryKey.columns)
                strs.add(c.name);
            throw new SQLError(LDatabase.LACK_OF_PRIMARY_KEY.toString()
                    + " ("
                    + strs.toString(", ") + ")");
        }
        Update upd = db.update(fullName, cond.toString());

        for (DbColumn col : columns)
            if (col.modified)
                upd.arg(col.name, col.value);
        return upd;
    }

    public QueryRows update(Database db, DbColumn<Self, ?>... keys) throws SQLException {
        Update upd = updateQuery(db, keys);
        QueryRows rows = upd.execute();

        QueryRow row = rows.first();
        if (row != null)
            deserialize(row);
        return rows;
    }

    public QueryRows merge(DbColumn<Self, ?>... keys) throws SQLException {
        return merge(null, keys);
    }

    public QueryRows merge(Database db, DbColumn<Self, ?>... keys) throws SQLException {
        Merge merge = mergeQuery(db, keys);
        QueryRows rows = merge.execute();
        QueryRow row = rows.first();
        if (row != null)
            deserialize(row);
        return rows;
    }

    public Merge mergeQuery(Database db, DbColumn<Self, ?>... keys) throws SQLException {
        if (db == null)
            db = db();

        if (keys == null || keys.length == 0)
            keys = primaryKey.columns;

        Merge merge = db.merge(fullName, keys[0].name);

        for (DbColumn col : columns)
            if (col.modified)
                merge.arg(col.name, col.value);

        return merge;
    }

    public Self clear() {
        for (DbColumn col : columns) {
            col.modified = false;
            col.value = null;
        }
        return (Self) this;
    }

    public Self deserialize(QueryRow row) throws SQLException {

        for (DbColumn col : columns)
            if (row.has(col.name))
                if (col.nullable)
                    col.value = row.getObj(col.name, null);
                else
                    col.value = row.getObj(col.name);
        return (Self) this;
    }
}

package com.database.orm;

import com.database.QueryRow;
import com.database.QueryRows;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.queries.StoredProc;
import com.exceptions.ThrowableException;
import com.utils.reflections.TClass;
import java.sql.SQLException;

public abstract class DbSchema<Main extends DbMain> {

    public DbFunction functions;
    public DbSequence sequences;
    public DbTableOrView tables[];
    public DbTrigger triggers;
    private final Class<Main> cls;

    public DbSchema(Class<Main> cls, String name) {
        this.cls = cls;
    }

    public static <T> T call(Class<? extends DbSchema> cls, String name, Class<T> returnTtype, Object... params) {
        try {
            DbSchema schema = new TClass<DbSchema>(cls).newInstance(null);
            StoredProc proc = schema.db().storedProc(name);
            if (params != null)
                for (Object o : params)
                    proc.add(o);
            QueryRows rows = proc.execute();

            QueryRow row = rows.first();
            return row != null ? (T) row.getObj(0) : null;
        } catch (SQLException e) {
            throw new ThrowableException(e);
        }
    }

    public PostgreSQL db() {
        return new TClass<>(cls).newInstance(null).db();
    }

}

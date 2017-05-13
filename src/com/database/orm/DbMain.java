package com.database.orm;

import com.database.drivers.postgresql.PostgreSQL;
import com.utils.reflections.TClass;

public abstract class DbMain<DB extends PostgreSQL> {

    public DbFunction functions;
    public DbSequence sequences;
    public DbTableOrView tables[];
    public DbTrigger triggers;
    private final Class<DB> cls;

    public DbMain(Class<DB> cls, String name) {
        this.cls = cls;
    }

    public DB db() {
        return new TClass<>(cls).newInstance(null);
    }

}

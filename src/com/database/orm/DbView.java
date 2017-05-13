package com.database.orm;

public abstract class DbView<Schema extends DbSchema, Self extends DbView>
        extends DbTableOrView<Schema, Self> {

    public DbView(Class<Schema> cls, String schemaName, String name, String type) {
        super(cls, schemaName, name, type);
    }

}

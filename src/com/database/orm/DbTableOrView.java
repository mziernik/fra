package com.database.orm;

import com.database.drivers.postgresql.PostgreSQL;
import com.utils.reflections.TClass;
import java.util.*;

public abstract class DbTableOrView<Schema extends DbSchema, Self extends DbTableOrView>
        implements Iterable<DbColumn<Self, ?>> {

    public final List<DbColumn<Self, ?>> columns = new LinkedList<>();
    public final String name;
    public final Class<Schema> schemaClass;
    public final String schemaName;
    public final String fullName;

    public DbTableOrView(Class<Schema> cls, String schemaName, String name, String type) {
        this.name = name;
        this.schemaClass = cls;
        this.schemaName = schemaName;
        this.fullName = schemaName + "." + name;
    }

    public <DataType> DbColumn<Self, DataType> tempColumn(
            Class<? extends DataType> dataType, String name, String query) {
        DbColumn col = new DbColumn(this, name);
        col.tempQuery = query;
        return col;
    }

    public PostgreSQL db() {
        return new TClass<>(schemaClass).newInstance(null).db();
    }

    @Override
    public String toString() {
        return schemaName + "." + name;
    }

    @Override
    public Iterator<DbColumn<Self, ?>> iterator() {
        return columns.iterator();
    }
}

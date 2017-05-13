package com.database.elements;

import java.util.LinkedHashMap;

/**
 * Miłosz Ziernik 2014/06/21
 */
public class MetaTable {

    public final LinkedHashMap<String, MetaColumn> columns = new LinkedHashMap<>();

    public final MetaSchema schema;
    public final String name;
    public String type;
    public final boolean meta;

    public MetaTable(MetaSchema schema, boolean meta, String name) {
        this.schema = schema;
        this.name = name;
        this.meta = meta;
        schema.tables.put(name, this);
    }

    @Override
    public String toString() {
        return schema.toString() + "." + name;
    }

}

package com.database.elements;

import java.util.LinkedHashMap;

public class MetaSchema {

    public final MetaCatalog catalog;
    public final LinkedHashMap<String, MetaTable> tables = new LinkedHashMap<>();
    public final LinkedHashMap<String, MetaFunction> functions = new LinkedHashMap<>();

    public final String name;
    public final boolean meta;

    public MetaSchema(MetaCatalog catalog, boolean meta, String name) {
        this.catalog = catalog;
        this.name = name;
        this.meta = meta;
        catalog.schemas.put(name, this);
    }

    @Override
    public String toString() {
        return name;
    }

}

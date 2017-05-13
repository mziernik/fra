package com.database.elements;

import java.util.LinkedHashMap;

/**
 * Miłosz Ziernik 2014/06/21
 */
public class MetaFunction {

    public final LinkedHashMap<String, MetaColumn> columns = new LinkedHashMap<>();

    public final MetaSchema schema;
    public final String name;
    public String type;
    public String specificName;

    public MetaFunction(MetaSchema schema, String name) {
        this.schema = schema;
        this.name = name;
        schema.functions.put(name, this);
    }

    @Override
    public String toString() {
        return schema.toString() + "." + name;
    }

}

package com.database.elements;

public class MetaColumn {

    public final MetaTable table;
    public final String name;
    public String type;
    public String defaultValue;
    public Boolean primaryKey;
    public Boolean nullable;
    public Integer size;
    public Boolean autoIncrement;

    public MetaColumn(MetaTable table, String name) {
        this.table = table;
        this.name = name;
        table.columns.put(name, this);
    }

    @Override
    public String toString() {
        return table.toString() + "." + name;
    }
}

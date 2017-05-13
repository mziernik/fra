package com.database.orm;

public class DbColumn<Table extends DbTableOrView, DataType extends Object> {

    public DbIndex[] indexes;
    public DbForeignKey[] foreignKeys;
    public DbCheck[] checks;
    public DbUnique uniques;
    public final String name;
    DataType value;
    public String tempQuery;
    public final Table table;
    public final boolean nullable;

    protected boolean modified;

    public DbColumn(Table table, String name, int dataType, String dataTypeName,
            String defValue, boolean nullable, boolean autoIncrement) {
        table.columns.add(this);
        this.table = table;
        this.name = name;
        this.nullable = nullable;
    }

    DbColumn(Table table, String name) {
        this.table = table;
        this.name = name;
        nullable = false;
    }

    @Override
    public String toString() {
        return name;
    }

    public DataType getValue() {
        return value;
    }

    public void setValue(Object obj) {
        value = (DataType) obj;
        modified = true;
    }

}

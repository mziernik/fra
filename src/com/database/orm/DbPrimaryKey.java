package com.database.orm;

public class DbPrimaryKey<Table extends DbTableOrView> {

    public final DbColumn<Table, ?>[] columns;
    public final String name;

    public DbPrimaryKey(String name, DbColumn<Table, ?>... columns) {
        this.columns = columns;
        this.name = name;
    }

}

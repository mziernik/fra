package com.database.orm;

import com.database.QueryRows;
import com.utils.reflections.TClass;
import java.sql.SQLException;

public class DbColumnFK<Table extends DbTableOrView, DataType extends Object, FKTable extends DbTable>
        extends DbColumn<Table, DataType> {

    private final Class<FKTable> fkTableClass;
    private final String fkColumnName;

    public DbColumnFK(Table table, String name, int dataType, String dataTypeName,
            String defValue, boolean nullable, boolean autoIncrement,
            Class<FKTable> fkTableClass, String fkColumnName) {
        super(table, name, dataType, dataTypeName, defValue, nullable, autoIncrement);

        this.fkTableClass = fkTableClass;
        this.fkColumnName = fkColumnName;
    }

    public FKTable getForeign() throws SQLException {
        FKTable fktable = new TClass<>(fkTableClass).newInstance(null);

        QueryRows rows = super.table.db().execute("SELECT * FROM " + fktable.fullName + " WHERE "
                + fkColumnName + " = ?\n"
                + "LIMIT 1", getValue());

        fktable.deserialize(rows.first());

        return fktable;
    }

}

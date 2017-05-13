package com.database.orm.generator;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Collection;
import java.util.Date;

class DColumn {

    final DTable table;
    final DView view;
    final String name;
    final String fieldName;
    final String typeClassName;
    private final Class<?> cls;
    final int dataType;
    final String typeName;
    final String def;
    final boolean nullable;
    final boolean autoIncrement;

    DColumn(DTableView parent, ResultSet columns) throws SQLException {
        this.table = parent instanceof DTable ? (DTable) parent : null;
        this.view = parent instanceof DView ? (DView) parent : null;

        name = columns.getString("COLUMN_NAME");
        parent.columns.put(name, this);
        String cls = StrUtils.formatMethodName(name);
        this.fieldName = cls;
        dataType = columns.getInt("DATA_TYPE");
        this.cls = toClass(dataType);
        typeName = columns.getString("TYPE_NAME");
        def = columns.getString("COLUMN_DEF");
        nullable = Utils.strBool(columns.getString("NULLABLE"), false);
        autoIncrement = Utils.strBool(columns.getString("IS_AUTOINCREMENT"), false);

        String typeClassName = this.cls.getCanonicalName();
        if (typeClassName.startsWith("java.lang."))
            typeClassName = typeClassName.substring("java.lang.".length());

        this.typeClassName = typeClassName;
    }

    Class<?> toClass(int type) {
        Class<?> result = Object.class;

        switch (type) {
            case Types.CHAR:
            case Types.VARCHAR:
            case Types.LONGVARCHAR:
                result = String.class;
                break;

            case Types.NUMERIC:
            case Types.DECIMAL:
                result = java.math.BigDecimal.class;
                break;

            case Types.BIT:
                result = Boolean.class;
                break;

            case Types.TINYINT:
                result = Byte.class;
                break;

            case Types.SMALLINT:
                result = Short.class;
                break;

            case Types.INTEGER:
                result = Integer.class;
                break;

            case Types.BIGINT:
                result = Long.class;
                break;

            case Types.REAL:
            case Types.FLOAT:
                result = Float.class;
                break;

            case Types.DOUBLE:
                result = Double.class;
                break;

            case Types.BINARY:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
                result = Byte[].class;
                break;

            case Types.DATE:
                result = java.sql.Date.class;
                break;

            case Types.TIME:
                result = java.sql.Time.class;
                break;

            case Types.TIMESTAMP:
                result = Date.class;
                break;

            case Types.ARRAY:
                result = Collection.class;
                break;
        }

        return result;
    }

}

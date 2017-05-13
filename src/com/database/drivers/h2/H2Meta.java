package com.database.drivers.h2;

import com.utils.Utils;
import com.utils.Is;
import com.database.Database;
import com.database.QueryRows;
import com.database.drivers.DbMeta;
import com.database.elements.*;
import java.lang.reflect.Array;
import java.sql.*;

import java.util.*;

public class H2Meta extends DbMeta {

    public H2Meta(Database db) {
        super(db);
    }

    @Override
    public MetaDbStructure getStructure() throws SQLException {
        MetaDbStructure result = new MetaDbStructure();
        DatabaseMetaData meta = db.getMetaData();

        try (ResultSet rs = meta.getCatalogs()) {
            while (rs.next())
                getCatalog(result, rs.getString("table_cat"));
        }

        try (ResultSet rs = meta.getSchemas()) {
            while (rs.next())
                getSchema(getCatalog(result,
                        rs.getString("table_catalog")),
                        rs.getString("table_schem"));
        }

        try (ResultSet rs = meta.getTables(null, null, null, null)) {
            /*
                table_cat
                table_schem
                table_name
                table_type
                remarks
             */
            while (rs.next()) {
                MetaTable table = getTable(getSchema(getCatalog(result,
                        rs.getString("table_cat")),
                        rs.getString("table_schem")),
                        rs.getString("table_name"));
                table.type = rs.getString("table_type");
            }

        }

        try (ResultSet rs = meta.getColumns(null, null, null, null)) {

            while (rs.next()) {
                /*
                TABLE_CAT
            TABLE_SCHEM
            TABLE_NAME
            COLUMN_NAME
            DATA_TYPE
            TYPE_NAME
            COLUMN_SIZE
            BUFFER_LENGTH
            DECIMAL_DIGITS
            NUM_PREC_RADIX
            NULLABLE
            REMARKS
            COLUMN_DEF
            SQL_DATA_TYPE
            SQL_DATETIME_SUB
            CHAR_OCTET_LENGTH
            ORDINAL_POSITION
            IS_NULLABLE
            SCOPE_CATLOG
            SCOPE_SCHEMA
            SCOPE_TABLE
            SOURCE_DATA_TYPE
            IS_AUTOINCREMENT
                 */
                MetaTable table = getTable(getSchema(getCatalog(result,
                        rs.getString("TABLE_CAT")),
                        rs.getString("TABLE_SCHEM")),
                        rs.getString("table_name"));

                MetaColumn col = new MetaColumn(table,
                        rs.getString("COLUMN_NAME")
                );
                col.type = rs.getString("TYPE_NAME");
                col.nullable = rs.getBoolean("IS_NULLABLE");
                col.size = rs.getInt("COLUMN_SIZE");
                col.autoIncrement = rs.getBoolean("IS_AUTOINCREMENT");
            }

        }

        try (ResultSet rs = meta.getColumnPrivileges(null, null, null, null)) {
            while (rs.next()) {

                MetaTable table = getTable(getSchema(getCatalog(result,
                        rs.getString("TABLE_CAT")),
                        rs.getString("TABLE_SCHEM")),
                        rs.getString("table_name"));

            }
        }

        try (ResultSet rs = meta.getColumnPrivileges(null, null, null, null)) {
            while (rs.next()) {

                MetaTable table = getTable(getSchema(getCatalog(result,
                        rs.getString("TABLE_CAT")),
                        rs.getString("TABLE_SCHEM")),
                        rs.getString("table_name"));

            }
        }

        try (ResultSet rs = meta.getProcedures(null, null, null)) {
            while (rs.next()) {
                /*
                    procedure_cat
                    procedure_schem
                    procedure_name
                    ?column?
                    ?column?
                    ?column?
                    remarks
                    procedure_type
                    specific_name
                 */
                // printColumNames(rs);
                MetaFunction funct = new MetaFunction(getSchema(getCatalog(result,
                        rs.getString("procedure_cat")),
                        rs.getString("procedure_schem")),
                        rs.getString("procedure_name"));
                funct.type = rs.getString("procedure_type");
                funct.specificName = rs.getString("specific_name");
            }
        }

        meta.getIdentifierQuoteString();

        return result;
    }

    @Override
    public Collection<Object> processArray(Object obj) throws SQLException {
        if (obj != null && obj.getClass().isArray()) {
            LinkedList<Object> list = new LinkedList<>();
            for (int i = 0; i < Array.getLength(obj); i++)
                list.add(Array.get(obj, i));
            return list;
        }

        return Utils.asList(obj);
    }

    @Override
    public Map<Object, Object> processMap(Object obj) throws SQLException {
        throw new UnsupportedOperationException();
    }

}

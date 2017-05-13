package com.database.drivers.sqlite;

import com.database.Database;
import com.database.drivers.DbMeta;
import com.database.elements.*;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import java.sql.*;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 06 listopada 2015
 * @encoding UTF-8
 */
public class SQLiteMeta extends DbMeta {

    public SQLiteMeta(Database db) {
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

            }

        }

        try (ResultSet rs = meta.getFunctions(null, null, null)) {
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
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Map<Object, Object> processMap(Object obj) throws SQLException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

}

package com.database.drivers.postgresql;

import com.dev.Dev;
import com.database.Database;
import com.database.drivers.DbMeta;
import com.database.elements.*;
import com.utils.Utils;
import com.utils.Is;
import com.utils.console.TConsole;
import java.lang.reflect.Array;
import java.sql.*;

import java.util.*;
import org.postgresql.jdbc.PgArray;
import org.postgresql.util.PGobject;

public class PgMeta extends DbMeta {

    public PgMeta(Database db) {
        super(db);
    }

    @Override
    public Object formatObject(Object obj) throws SQLException {

        if (obj == null)
            return null;

        //ToDo: Dopisać mapowanie typów postgresa np inet (lista w komentarzu na dole)
        if (obj instanceof PGobject) {
            PGobject pgObj = (PGobject) obj;
            return pgObj.getValue();
        }

        if (obj instanceof PgArray) {
            List<Object> list = new LinkedList<>();
            Object pgArr = ((PgArray) obj).getArray();

            if (pgArr != null && pgArr.getClass().isArray()) {
                int length = Array.getLength(pgArr);
                for (int i = 0; i < length; i++)
                    list.add(formatObject(Array.get(pgArr, i)));
                return list;
            }
        }

        if (obj.getClass().getName().startsWith("org.postgresql.jdbc."))
            Dev.warning("Nieobsłuzony typ danych PostgreSQL: " + obj.getClass().getName());

        return obj;
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
                String type = Utils.coalesce(rs.getString("table_type"), "").toLowerCase();

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
                col.type = rs.getString("TYPE_NAME");
                col.nullable = rs.getBoolean("IS_NULLABLE");
                col.size = rs.getInt("COLUMN_SIZE");
                col.autoIncrement = rs.getBoolean("IS_AUTOINCREMENT");

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

    void printColumNames(ResultSet rs) throws SQLException {
        ResultSetMetaData rsmeta = rs.getMetaData();
        for (int i = 0; i < rs.getMetaData().getColumnCount(); i++)
            TConsole.print(rsmeta.getColumnName(i + 1));
    }

    @Override
    public Collection<Object> processArray(Object obj) throws SQLException {

        List<Object> list = new LinkedList<>();

        if (obj instanceof Iterable)
            for (Object o : (Iterable) obj)
                list.add(o);
        else if (obj instanceof PgArray)
            try (ResultSet rs = ((PgArray) obj).getResultSet()) {
                while (rs.next())
                    list.add(rs.getObject(2));
            }
        else
            list.add(obj);

        return list;
    }

    @Override
    public Map<Object, Object> processMap(Object obj) throws SQLException {

        if (obj instanceof Map)
            return (Map<Object, Object>) obj;

        return null;
    }
}


/* 1 data_type_id Data Type Id java.lang.Integer int4 11

2 smallint_type Smallint Type java.lang.Integer int2 6
3 int_type Int Type java.lang.Integer int4 11
4 bigint_type Bigint Type java.lang.Long int8 20
5 decimal_type Decimal Type java.math.BigDecimal numeric 18

6 numeric_type Numeric Type java.math.BigDecimal numeric 12
7 real_type Real Type java.lang.Float float4 14
8 doubleprecision_type Doubleprecision Type java.lang.Double float8 24
9 serial_type Serial Type java.lang.Integer int4 11

10 bigserial_type Bigserial Type java.lang.Long int8 20
11 varchar_type Varchar Type java.lang.String varchar 30
12 char_type Char Type java.lang.String bpchar 30
13 text_type Text Type java.lang.String text 2147483647

14 bytea_type Bytea Type [B bytea 2147483647
15 date_type Date Type java.sql.Date date 13
16 time_type Time Type java.sql.Time time 15
17 timetz_type Timetz Type java.sql.Time timetz 21
18 timestamp_type Timestamp Type java.sql.Timestamp timestamp 29

19 timestamptz_type Timestamptz Type java.sql.Timestamp timestamptz 35
20 interval_type Interval Type org.postgresql.util.PGInterval interval 49
21 boolean_type Boolean Type java.lang.Boolean bool 1

22 point_type Point Type org.postgresql.geometric.PGpoint point 2147483647 23
linesegment_type Linesegment Type org.postgresql.geometric.PGlseg lseg
2147483647

24 box_type Box Type org.postgresql.geometric.PGbox box 2147483647
25 path_type Path Type org.postgresql.geometric.PGpath path 2147483647

26 polygon_type Polygon Type org.postgresql.geometric.PGpolygon polygon
2147483647 27 circle_type Circle Type org.postgresql.geometric.PGcircle
circle 2147483647

28 cidr_type Cidr Type java.lang.Object cidr 2147483647
29 inet_type Inet Type java.lang.Object inet 2147483647
30 macaddr_type Macaddr Type java.lang.Object macaddr 2147483647

31 bit2_type Bit2 Type java.lang.Boolean bit 2
32 bitvarying5_type Bitvarying5 Type java.lang.Object varbit 5
 */

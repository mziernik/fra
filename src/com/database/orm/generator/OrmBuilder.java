package com.database.orm.generator;

import com.utils.Path;
import com.utils.console.TConsole;
import com.context.AppContext;
import com.dev.Dev;
import com.database.QueryRow;
import com.database.drivers.postgresql.PostgreSQL;
import com.json.JObject;
import com.lang.LDatabase;
import com.utils.text.StrWriter;
import com.xml.XML;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.*;
import java.util.*;

public class OrmBuilder {

    public final Path destination;

    private final PostgreSQL db;
    private final String rootPackage;
    private final String dbClass;

    public OrmBuilder(PostgreSQL db, String dbClass, String pckg) throws FileNotFoundException {

        if (!AppContext.sourcesPath.exists())
            throw new FileNotFoundException(Dev.srcDir);

        this.dbClass = dbClass;
        this.db = db;
        this.destination = AppContext.sourcesPath;
        this.rootPackage = pckg;
    }

    public String generateStructure() throws SQLException, IOException {

        TConsole.print(LDatabase.CREATING_STRUCTURE_PACKAGE.toString(rootPackage));

        StrWriter writer = new StrWriter();

        DatabaseMetaData meta = db.getMetaData();

        DMain main = new DMain(db, dbClass, rootPackage);

        try (ResultSet rst = meta.getSchemas()) {
            while (rst.next()) {
                String name = rst.getString("table_schem");
                if (name.equals("information_schema") || name.equals("pg_catalog"))
                    continue;

                main.schemas.put(name, new DSchema(main, name));

            }
        }

        try (ResultSet rst = meta.getTables(null, null, null, null)) {
            while (rst.next()) {
                String type = rst.getString("table_type");
                if ("TABLE".equals(type))
                    new DTable(main.schemas.get(rst.getString("table_schem")), meta, rst);

                if ("VIEW".equals(type))
                    new DView(main.schemas.get(rst.getString("table_schem")), meta, rst);
            }
        }

        try (ResultSet rst = meta.getColumns(null, null, null, null)) {
            while (rst.next()) {
                DSchema schema = main.schemas.get(rst.getString("TABLE_SCHEM"));
                if (schema == null)
                    continue;

                DTable table = schema.tables.get(rst.getString("TABLE_NAME"));
                if (table != null)
                    new DColumn(table, rst);

                DView vv = schema.views.get(rst.getString("TABLE_NAME"));
                if (vv != null)
                    new DColumn(vv, rst);

            }
        }

        for (DSchema sch : main.schemas.values())
            for (DTable tbl : sch.tables.values())
                tbl.processIndexes(meta);

        try (ResultSet rst = meta.getPrimaryKeys(null, null, null)) {
            while (rst.next())
                new DPrimaryKey(main, rst);
        }

        try (ResultSet rst = meta.getExportedKeys(null, null, null)) {
            while (rst.next())
                new DForeignKey(main, rst);
        }

        try {
            for (QueryRow row : db.execute("SELECT n.nspname as schema,\n"
                    + "  p.proname as name,\n"
                    + "  pg_catalog.pg_get_function_result(p.oid) as result,\n"
                    + "  pg_catalog.pg_get_function_arguments(p.oid) as arguments,\n"
                    + "  pg_get_functiondef(p.oid) AS func_def\n"
                    + "FROM pg_catalog.pg_proc p\n"
                    + "     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace\n"
                    + "WHERE pg_catalog.pg_function_is_visible(p.oid)\n"
                    + "      AND n.nspname <> 'pg_catalog'\n"
                    + "      AND n.nspname <> 'information_schema'\n"
                    + "ORDER BY 1, 2, 4;"))

                new DFunction(main, row);
        } catch (Exception e) {
            TConsole.printErr(e);
        }
        //----------------------------------------------------------------------
        File srcs = destination.toFile();

        main.build(srcs);

        for (DSchema sch : main.schemas.values())
            if (!sch.isEmpty())
                sch.build(srcs);

        return writer.toString();

    }

    void printMeta(ResultSet rst) throws SQLException {
        ResultSetMetaData meta = rst.getMetaData();
        while (rst.next())
            for (int i = 0; i < meta.getColumnCount(); i++)
                TConsole.print((i + 1) + ". "
                        + meta.getColumnName(i + 1)
                        + ": " + rst.getObject(i + 1));
    }

    public static Class parseType(String name) {
        if (name == null || name.isEmpty())
            return null;

        switch (name.trim().toLowerCase()) {
            case "void":
                return Void.TYPE;

            case "boolean":
            case "bool":
                return Boolean.class;
            case "boolean[]":
            case "bool[]":
                return boolean[].class;

            case "character":
            case "char":
                return Character.class;

            case "byte":
                return Byte.class;

            case "byte[]":
            case "bytea":
                return byte[].class;

            case "hstore":
            case "json":
                return JObject.class;

            case "text":
            case "cstring":
                return String.class;

            case "text[]":
            case "cstring[]":
                return String[].class;

            case "smallint":
            case "smallserial":
                return Short.class;

            case "smallint[]":
            case "smallserial[]":
                return short[].class;

            case "int":
            case "int4":
            case "integer":
            case "serial":
                return Integer.class;

            case "int[]":
            case "int4[]":
            case "integer[]":
            case "serial[]":
                return int[].class;

            case "money":
            case "numeric":
                return Double.class;

            case "money[]":
            case "numeric[]":
                return double[].class;

            case "int8":
            case "bigint":
            case "bigserial":
                return Long.class;

            case "int8[]":
            case "bigint[]":
            case "bigserial[]":
                return long[].class;

            case "float":
            case "float4":
            case "real":
                return Float.class;

            case "float[]":
            case "float4[]":
            case "real[]":
                return float[].class;

            case "double":
            case "float8":
                return Double.class;

            case "double[]":
            case "float8[]":
                return double.class;

            case "date":
            case "time":
            case "timestamp":
            case "timestamp without time zone":
            case "timestamp with time zone":
                return java.util.Date.class;

            case "date[]":
            case "time[]":
            case "timestamp[]":
            case "timestamp without time zone[]":
            case "timestamp with time zone[]":
                return java.util.Date[].class;

            case "uuid":
                return UUID.class;
            case "uuid[]":
                return UUID[].class;

            case "xml":
                return XML.class;

            case "xml[]":
                return XML[].class;
        }
        return Object.class;
    }

}

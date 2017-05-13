package com.database.drivers.postgresql;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.database.DBConnectionData;
import com.database.QueryRow;
import com.json.JObject;
import com.utils.JavaFile;
import com.utils.collections.*;
import com.utils.console.TConsole;
import com.xml.XML;
import java.io.File;
import java.io.IOException;
import java.sql.*;
import java.util.*;

/**
 * Generator klas javy (encji) odzwierciedlających strukturę bazy danych
 * Mapowanie relacyjno obiektowe - DAO
 *
 * @author user
 */
public class DaoGenerator {

    private final PostgreSQL db;
    private final File srcsDir;
    private final String packageName;

    public DaoGenerator(PostgreSQL db, File srcsDir, String packageName) {
        this.db = db;
        this.srcsDir = srcsDir;
        this.packageName = packageName;
    }

    final Map<String, DSchema> schemas = new LinkedHashMap<>();

    public void generateStructure() throws SQLException, IOException {

        DatabaseMetaData meta = db.getMetaData();

        //  printMeta(primaryKeys);
        try (ResultSet rst = meta.getTables(null, null, null, null)) {
            while (rst.next()) {

                if (!"TABLE".equals(rst.getString("table_type")))
                    continue;

                DTable tbl = new DTable(meta, rst);
            }
        }

        try (ResultSet rst = meta.getColumns(null, null, null, null)) {
            while (rst.next()) {
                DSchema schema = schemas.get(rst.getString("TABLE_SCHEM"));
                if (schema == null)
                    continue;
                DTable table = schema.tables.get(rst.getString("TABLE_NAME"));
                if (table == null)
                    continue;
                new DColumn(table, rst);
            }
        }

        for (DSchema sch : schemas.values())
            for (DTable tbl : sch.tables.values())
                tbl.processIndexes(meta);

        try (ResultSet rst = meta.getPrimaryKeys(null, null, null)) {
            while (rst.next())
                new DPrimaryKey(rst);
        }

        try (ResultSet rst = meta.getExportedKeys(null, null, null)) {
            while (rst.next())
                new DForeignKey(rst);
        }

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

            new DFunction(row);

        //----------------------------------------------------------------------
        for (DSchema sch : schemas.values()) {

            JavaFile jfile = new JavaFile(packageName, sch.className)
                    .import_("com.database.dao.*");

            JavaFile.JavaClass jcls = jfile.addClass(sch.className)
                    .modifier("public")
                    .extend("DbSchema");

            JavaFile.JavaMethod cstr = jcls.addMethod(sch.className, null)
                    .modifier("public");
            cstr.body.append("super(").escape(sch.name).append(");");

            for (DFunction f : sch.functions.values()) {

                JavaFile.JavaMethod mth = jcls.addMethod(f.result.getSimpleName(), f.className)
                        .modifier("public")
                        .modifier("static");

                for (Pair<String, Class> p : f.arguments) {
                    mth.argument(p.second.getSimpleName(), p.first);
                    jfile.importIfRequired(p.second);
                }

                if (f.result != Void.TYPE)
                    mth.body.append("return ");

                String attrType = f.result == Void.TYPE
                        ? "Void.TYPE"
                        : f.result.getSimpleName() + ".class";

                mth.body.append("call(").escape(sch.name + "." + f.name).append(", ")
                        .append(attrType);

                for (Pair<String, Class> p : f.arguments)
                    mth.body.append(", ").append(p.first);
                mth.body.append(");");
            }

            jfile.savePckg(srcsDir);

            //-------------------------------------------------------
            for (DTable tbl : sch.tables.values()) {

                jfile = new JavaFile(packageName + ".tables"
                        + ("public".equals(tbl.schema.name) ? "" : "." + tbl.schema.name),
                        tbl.className)
                        .import_("com.database.dao.*")
                        .import_(packageName + "." + sch.className)
                        .import_("java.sql.*")
                        .import_("java.math.*");

                jcls = jfile.addClass(tbl.className)
                        .modifier("public")
                        .extend("DbTable<" + tbl.schema.className + ", " + tbl.className + ">");

                cstr = jcls.addMethod(tbl.className, null)
                        .modifier("public"); // konstruktor

                for (DColumn col : tbl.columns.values())
                    jcls.addField("DbColumn<" + tbl.className + ", "
                            + col.cls.getSimpleName() + ">", col.className)
                            .modifier("public final");

                cstr.body.append("super(")
                        .append(tbl.schema.className).append(".class").append(", ")
                        .escape(tbl.schema.name).append(", ")
                        .escape(tbl.name).append(", ")
                        .escape(tbl.type)
                        .append(");\n");

                for (DColumn col : tbl.columns.values()) {
                    cstr.body
                            .append(col.className).append(" = ")
                            .append("new DbColumn<>(this, ")
                            .escape(col.name).append(", ")
                            .escape(col.dataType).append(", ")
                            .escape(col.typeName).append(", ")
                            .escape(col.def).append(", ")
                            .escape(col.nullable).append(", ")
                            .escape(col.autoIncrement)
                            .append(");\n");

                    jcls.addMethod(col.cls.getSimpleName(), "get"
                            + col.className.substring(0, 1).toUpperCase()
                            + col.className.substring(1))
                            .modifier("public").body
                            .append("return ").append(col.className).append(".value();");

                }

                if (tbl.pkey != null)
                    cstr.body.append("\nprimaryKey = new DbPrimaryKey(")
                            .escape(tbl.pkey.name).append(", ")
                            .append(tbl.pkey.colNames.toString(", "))
                            .append(");\n");

                for (DIndex idx : tbl.indexes.values())
                    cstr.body.append("\nindexes.add(new DbIndex(this, ")
                            .escape(idx.name).append(", ")
                            .append(idx.colNames.toString(", "))
                            .append("));");

                if (!tbl.indexes.isEmpty())
                    cstr.body.append("\n");

                DTable fktbl = null;
                for (DForeignKey fk : tbl.foreignKeys)
                    cstr.body.append("\nforeignKeys.add(new DbForeignKey(")
                            .escape(fk.name).append(", ")
                            .append(fk.local.className).append(", ")
                            .append(fk.foreign.table.className).append(".class, ")
                            .escape(fk.foreign.name)
                            .append("));");
                if (fktbl != null)
                    cstr.body.append("\n");

                jfile.savePckg(srcsDir);
            }

        }
    }

    void printMeta(ResultSet rst) throws SQLException {
        ResultSetMetaData meta = rst.getMetaData();
        while (rst.next())
            for (int i = 0; i < meta.getColumnCount(); i++)
                TConsole.print((i + 1) + ". "
                        + meta.getColumnName(i + 1)
                        + ": " + rst.getObject(i + 1));
    }

    private class DPrimaryKey {

        final List<DColumn> columns = new LinkedList<>();
        final Strings colNames = new Strings();
        final String name;

        private DPrimaryKey(ResultSet rs) throws SQLException {

            name = rs.getString("pk_name");

            DSchema sch = schemas.get(rs.getString("table_schem"));
            if (sch == null)
                return;
            DTable tbl = sch.tables.get(rs.getString("table_name"));
            if (tbl == null)
                return;

            DColumn c = tbl.columns.get(rs.getString("column_name"));

            if (c == null)
                return;

            if (tbl.pkey == null)
                tbl.pkey = this;

            tbl.pkey.columns.add(c);
            tbl.pkey.colNames.add(c.className);

        }

    }

    private class DForeignKey {

        final String name;
        DColumn local;
        DColumn foreign;

        private DForeignKey(ResultSet rs) throws SQLException {

            //    printMeta(rs);
            name = rs.getString("fk_name");

            DSchema sch = schemas.get(rs.getString("pktable_schem"));
            if (sch == null)
                return;
            DTable ftbl = sch.tables.get(rs.getString("pktable_name"));
            if (ftbl == null)
                return;
            foreign = ftbl.columns.get(rs.getString("pkcolumn_name"));

            sch = schemas.get(rs.getString("fktable_schem"));
            if (sch == null)
                return;

            ftbl = sch.tables.get(rs.getString("fktable_name"));

            if (ftbl == null)
                return;

            local = ftbl.columns.get(rs.getString("fkcolumn_name"));
            ftbl.foreignKeys.add(this);
        }

    }

    private class DFunction {

        final String name;
        final String className;
        final String define;
        final Class result;
        final Pairs<String, Class> arguments = new Pairs<>();

        private DFunction(QueryRow row) throws SQLException {
            name = row.getStr("name", null);

            String fname = StrUtils.formatMethodName(name);
            String cname = fname;

            define = row.getStr("func_def", "");

            result = parseType(row.getStr("result", null));

            DSchema sch = schemas.get(row.getStr("schema", null));

            int idx = 1;
            if (sch != null)
                while (sch.functions.get(cname) != null)
                    cname = fname + ++idx;

            className = cname;

            if (sch == null)
                return;

            String[] args = row.getStr("arguments", null).split("\\,");

            int param = 0;
            for (String arg : args) {
                String[] p = arg.split(" ");
                if (p.length != 2)
                    continue;

                ++param;
                String name = p[0];
                if (name.trim().isEmpty())
                    name = "param" + param;
                arguments.add(StrUtils.formatMethodName(name), parseType(p[1]));

            }

            for (String s : define.split("\\n"))
                if (s.trim().equals("LANGUAGE c"))
                    return;

            sch.functions.put(cname, this);

        }

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

    private class DIndex {

        final List<DColumn> columns = new LinkedList<>();
        final Strings colNames = new Strings();
        final String name;

        private DIndex(ResultSet rs) throws SQLException {
            name = rs.getString("index_name");
        }

    }

    private class DSchema {

        public final Map<String, DTable> tables = new LinkedHashMap<>();
        final Map<String, DFunction> functions = new LinkedHashMap<>();
        public final String name;
        final String className;
        final String pckg;

        private DSchema(String name) {
            this.name = name;
            String cls = StrUtils.formatMethodName(name);
            this.pckg = cls;
            if (cls.startsWith("_"))
                cls = cls.substring(1);
            cls = cls.substring(0, 1).toUpperCase() + cls.substring(1);
            this.className = cls + "Schema";
        }

    }

    private class DTable {

        final List<DForeignKey> foreignKeys = new LinkedList<>();
        public final Map<String, DColumn> columns = new LinkedHashMap<>();
        final String catalog;
        final DSchema schema;
        final String name;
        final String type;
        final String className;
        DPrimaryKey pkey;
        final Map<String, DIndex> indexes = new LinkedHashMap<>();

        private DTable(DatabaseMetaData meta, ResultSet tables) throws SQLException {
            catalog = tables.getString("table_cat");
            name = tables.getString("table_name");
            type = tables.getString("table_type");
            String s = tables.getString("table_schem");
            DSchema schema = schemas.get(s);
            if (schema == null) {
                schema = new DSchema(s);
                schemas.put(s, schema);
            }
            this.schema = schema;
            schema.tables.put(name, this);

            String cls = StrUtils.formatMethodName(name);
            cls = cls.substring(0, 1).toUpperCase() + cls.substring(1);

            s = StrUtils.formatMethodName(schema.name);
            s = s.substring(0, 1).toUpperCase() + s.substring(1);

            this.className = ("public".equalsIgnoreCase(schema.name) ? "" : s) + cls;

        }

        void processIndexes(DatabaseMetaData meta) throws SQLException {
            ResultSet indexInfo = meta.getIndexInfo(null, schema.name, name, false, false);
            while (indexInfo.next()) {
                String idxName = indexInfo.getString("index_name");
                DColumn col = columns.get(indexInfo.getString("column_name"));
                if (col == null)
                    continue;
                DIndex idx = indexes.get(idxName);
                if (idx == null) {
                    idx = new DIndex(indexInfo);
                    indexes.put(idxName, idx);
                }
                idx.columns.add(col);
                idx.colNames.add(col.className);
            }

        }
    }

    private class DColumn {

        final DTable table;
        final String name;
        final String className;
        final Class<?> cls;
        final int dataType;
        final String typeName;
        final String def;
        final boolean nullable;
        final boolean autoIncrement;

        private DColumn(DTable table, ResultSet columns) throws SQLException {
            this.table = table;
            name = columns.getString("COLUMN_NAME");
            table.columns.put(name, this);
            String cls = StrUtils.formatMethodName(name);
            this.className = cls;
            dataType = columns.getInt("DATA_TYPE");
            this.cls = toClass(dataType);
            typeName = columns.getString("TYPE_NAME");
            def = columns.getString("COLUMN_DEF");
            nullable = Utils.strBool(columns.getString("NULLABLE"), false);
            autoIncrement = Utils.strBool(columns.getString("IS_AUTOINCREMENT"), false);
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
                    result = java.sql.Timestamp.class;
                    break;
            }

            return result;
        }

    }

}

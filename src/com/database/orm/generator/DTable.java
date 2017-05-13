package com.database.orm.generator;

import com.database.orm.DbSchema;
import com.utils.JavaFile;
import com.utils.JavaFile.JavaClass;
import com.utils.JavaFile.JavaMethod;
import com.utils.console.TConsole;
import java.io.File;
import java.io.IOException;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;

class DTable extends DTableView {

    DTable(DSchema schema, DatabaseMetaData meta, ResultSet tables) throws SQLException {
        super(schema, meta, tables);

        TConsole.print("Tabela " + name);
    }

    @Override
    void build(JavaFile jSchema, File dir) throws IOException {
        JavaFile jTable = new JavaFile(pckg, className)
                .import_(DbSchema.class.getPackage().getName() + ".*");

        JavaClass jcls = jTable.addClass(className)
                .modifier("public")
                .extend("DbTable<" + schema.canonicalName + ", " + className + ">");

        JavaMethod cstr = jcls.addMethod(className, null)
                .modifier("public"); // konstruktor

        for (DColumn col : columns.values())
            jcls.addField("DbColumn<" + className + ", "
                    + col.typeClassName + ">", col.fieldName)
                    .modifier("public final");

        cstr.body.append("super(")
                .append(schema.canonicalName).append(".class").append(", ")
                .escape(schema.name).append(", ")
                .escape(name).append(", ")
                .escape(type)
                .append(");\n");

        jcls.addMethod(schema.main.db.getClass(), "db")
                .annotation("@Override")
                .modifier("public").body
                .append("return (")
                .append(JavaFile.getCanonicalName(schema.main.db.getClass()))
                .append(") super.db();");


        /*
                    @Override
    public DbManager db() {
        return (DbManager) super.db();
    }
         */
        for (DForeignKey fk : foreignKeys)
            jcls.addMethod(fk.foreign.table.canonicalName, "get"
                    + fk.local.fieldName.substring(0, 1).toUpperCase()
                    + fk.local.fieldName.substring(1)
                    + "FK")
                    .throw_(SQLException.class).body
                    .append("return new ")
                    .append(fk.foreign.table.canonicalName)
                    .append("()\n")
                    .append("\t.deserialize(db().execute(")
                    .append("\n\t\"SELECT * FROM ")
                    .append(fk.foreign.table.schema.name)
                    .append(".")
                    .append(fk.foreign.table.name)
                    .append(" WHERE ")
                    .append(fk.foreign.name)
                    .append(" = ? LIMIT 1\",\n\t")
                    .append(fk.local.fieldName)
                    .append(".getValue()).first());");
        /*
              return new Customers()
                .deserialize(db().execute(
                        "SELECT * FROM customers.customers WHERE customer_id = ? LIMIT 1",
                        this.customerId).first());
         */

        for (DColumn col : columns.values()) {
            cstr.body
                    .append(col.fieldName).append(" = ")
                    .append("new DbColumn<>(this, ")
                    .escape(col.name).append(", ")
                    .escape(col.dataType).append(", ")
                    .escape(col.typeName).append(", ")
                    .escape(col.def).append(", ")
                    .escape(col.nullable).append(", ")
                    .escape(col.autoIncrement)
                    .append(");\n");

            jcls.addMethod(col.typeClassName, "get"
                    + col.fieldName.substring(0, 1).toUpperCase()
                    + col.fieldName.substring(1))
                    .modifier("public").body
                    .append("return ").append(col.fieldName).append(".getValue();");

            jcls.addMethod(className, "set"
                    + col.fieldName.substring(0, 1).toUpperCase()
                    + col.fieldName.substring(1))
                    .argument(col.typeClassName, col.fieldName)
                    .modifier("public").body
                    .append("this.")
                    .append(col.fieldName).append(".setValue(")
                    .append(col.fieldName)
                    .append(");\nreturn this;");
        }

        if (pkey != null)
            cstr.body.append("\nprimaryKey = new DbPrimaryKey(")
                    .escape(pkey.name).append(", ")
                    .append(pkey.colNames.toString(", "))
                    .append(");\n");

        for (DIndex idx : indexes.values())
            cstr.body.append("\nindexes.add(new DbIndex(this, ")
                    .escape(idx.name).append(", ")
                    .append(idx.colNames.toString(", "))
                    .append("));");

        if (!indexes.isEmpty())
            cstr.body.append("\n");

        DTableView fktbl = null;
        for (DForeignKey fk : foreignKeys)
            cstr.body.append("\nforeignKeys.add(new DbForeignKey(")
                    .escape(fk.name).append(", ")
                    .append(fk.local.fieldName).append(",\n\t")
                    .append(fk.foreign.table.canonicalName).append(".class, ")
                    .escape(fk.foreign.name)
                    .append("));");

        if (fktbl != null)
            cstr.body.append("\n");

        jTable.savePckg(dir);
    }

}

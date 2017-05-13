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

class DView extends DTableView {

    DView(DSchema schema, DatabaseMetaData meta, ResultSet tables) throws SQLException {
        super(schema, meta, tables);

        TConsole.print("Widok " + name);
    }

    @Override
    void build(JavaFile jSchema, File dir) throws IOException {
        JavaFile jTable = new JavaFile(pckg, className)
                .import_(DbSchema.class.getPackage().getName() + ".*");

        JavaClass jcls = jTable.addClass(className)
                .modifier("public")
                .extend("DbView<" + schema.canonicalName + ", " + className + ">");

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
        }

        jTable.savePckg(dir);
    }

}

package com.database.orm.generator;

import com.utils.StrUtils;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.orm.DbSchema;
import com.utils.JavaFile;
import com.utils.collections.Pair;
import com.utils.console.TConsole;
import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

class DSchema {

    public final Map<String, DTable> tables = new LinkedHashMap<>();
    public final Map<String, DView> views = new LinkedHashMap<>();
    final Map<String, DFunction> functions = new LinkedHashMap<>();
    public final String name;
    final String className;
    final String pckg;
    final String canonicalName;
    final DMain main;

    DSchema(DMain main, String name) {
        this.name = name;
        this.main = main;
        String cls = name.equalsIgnoreCase("public") ? "main" : StrUtils.formatMethodName(name);
        this.pckg = main.pckg + "." + cls;
        cls = cls.substring(0, 1).toUpperCase() + cls.substring(1);
        this.className = cls;
        this.canonicalName = this.pckg + "." + this.className;

        TConsole.print("Schemat " + name);
    }

    boolean isEmpty() {
        return tables.isEmpty();
    }

    void build(File dir) throws IOException {

        JavaFile jSchema = new JavaFile(pckg, className);
        jSchema.import_(DbSchema.class.getPackage().getName() + ".DbSchema");

        JavaFile.JavaClass jcls = jSchema.addClass(className)
                .modifier("public")
                .extend("DbSchema<" + main.canonicalName + ">");

        JavaFile.JavaMethod cstr = jcls.addMethod(className, null)
                .modifier("public");
        cstr.body.append("super(")
                .append(main.canonicalName)
                .append(".class, ")
                .escape(name)
                .append(");");

        jcls.addMethod(main.db.getClass(), "db")
                .annotation("@Override")
                .modifier("public").body
                .append("return (")
                .append(JavaFile.getCanonicalName(main.db.getClass()))
                .append(") super.db();");

        for (DFunction f : functions.values()) {

            JavaFile.JavaMethod mth = jcls.addMethod(f.result, f.className)
                    .modifier("public")
                    .modifier("static");

            for (Pair<String, Class> p : f.arguments)
                mth.argument(p.second, p.first);

            if (f.result != Void.TYPE)
                mth.body.append("return ");

            String typeClassName = f.result.getCanonicalName();
            if (typeClassName.startsWith("java.lang."))
                typeClassName = typeClassName.substring("java.lang.".length());

            String attrType = f.result == Void.TYPE
                    ? "Void.TYPE"
                    : typeClassName + ".class";

            mth.body.append("call(")
                    .append(canonicalName).append(".class, ")
                    .escape(name + "." + f.name).append(", ")
                    .append(attrType);

            for (Pair<String, Class> p : f.arguments)
                mth.body.append(", ").append(p.first);
            mth.body.append(");");
        }

        jSchema.savePckg(dir);

        for (DTable tbl : tables.values())
            tbl.build(jSchema, dir);

        for (DView vv : views.values())
            vv.build(jSchema, dir);

    }

}

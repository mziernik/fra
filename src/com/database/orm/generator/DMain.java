package com.database.orm.generator;

import com.utils.StrUtils;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.orm.DbMain;
import com.json.Escape;
import com.utils.JavaFile;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

class DMain {

    final Map<String, DSchema> schemas = new LinkedHashMap<>();

    public final String name;
    final String className;
    final String pckg;
    final String canonicalName;
    final PostgreSQL db;
    private String ver;
    final String dbClass;

    DMain(PostgreSQL db, String dbClass, String rootPackage) throws SQLException {
        this.dbClass = dbClass;
        String s = db.connData().url;
        s = s.substring(s.lastIndexOf("/") + 1);
        this.name = s;
        this.db = db;
        String cls = name.equalsIgnoreCase("public") ? "main" : StrUtils.formatMethodName(name);
        this.pckg = rootPackage;
        cls = cls.substring(0, 1).toUpperCase() + cls.substring(1);
        this.className = cls;
        this.canonicalName = this.pckg + "." + this.className;

        ver = db.execute("SELECT version()").first().getStr(0);

    }

    void build(File dir) throws IOException {

        JavaFile java = new JavaFile(pckg, className);
        java.import_(DbMain.class.getPackage().getName() + ".DbMain");

        JavaFile.JavaClass jcls = java.addClass(className)
                .modifier("public")
                .extend("DbMain<" + dbClass + ">");

        jcls.addField("String", "VERSION")
                .modifier("public")
                .modifier("final")
                .modifier("static").body.append(Escape.escape(ver));

        JavaFile.JavaMethod cstr = jcls.addMethod(className, null)
                .modifier("public");
        cstr.body.append("super(")
                .append(dbClass)
                .append(".class, ")
                .escape(name)
                .append(");");

        java.savePckg(dir);

    }

}

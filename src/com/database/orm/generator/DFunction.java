package com.database.orm.generator;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.database.QueryRow;
import com.lang.LDatabase;
import com.utils.collections.Pairs;
import com.utils.console.TConsole;
import java.sql.SQLException;

class DFunction {

    final String name;
    final String className;
    final String define;
    final Class result;
    final Pairs<String, Class> arguments = new Pairs<>();

    DFunction(DMain gen, QueryRow row) throws SQLException {
        name = row.getStr("name", null);

        String fname = StrUtils.formatMethodName(name);
        String cname = fname;

        define = row.getStr("func_def", "");

        result = OrmBuilder.parseType(row.getStr("result", null));

        DSchema sch = gen.schemas.get(row.getStr("schema", null));

        int idx = 1;
        if (sch != null)
            while (sch.functions.get(cname) != null)
                cname = fname + ++idx;

        className = cname;

        if (sch == null)
            return;

        String[] args = row.getStr("arguments", null).split("\\,");

        //  arguments.add(sch.canonicalName + ".class");
        int param = 0;
        for (String arg : args) {
            String[] p = arg.trim().split(" ");
            if (p.length != 2)
                continue;

            ++param;
            String name = p[0];
            if (name.trim().isEmpty())
                name = "param" + param;
            arguments.add(StrUtils.formatMethodName(name), OrmBuilder.parseType(p[1]));

        }

        for (String s : define.split("\\n"))
            if (s.trim().equals("LANGUAGE c"))
                return;

        sch.functions.put(cname, this);

        TConsole.print(LDatabase.FUNCTION.toString(name));

    }

}

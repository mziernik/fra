package com.database.queries;

import com.utils.Utils;
import com.utils.Is;
import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.database.queries.builder.*;
import com.exceptions.SQLError;
import com.utils.collections.Strings;
import java.util.LinkedList;
import java.util.Objects;

public class Upsert extends QueryBuilder<Upsert> {

    private final String table;
    private final String[] keyColumns;
    protected final Strings returning = new Strings()
            .trim(true)
            .unique(true)
            .nonEmpty(true);

    public Upsert(Database db, String table, String... keyColumns) {
        super(db);
        this.table = table;
        this.keyColumns = keyColumns;
    }

    @Override
    public Upsert arg(String name, Object value) {
        return super.arg(name, value);
    }

    public Upsert addReturningColumn(String column) {
        returning.add(column);
        return this;
    }

    @Override
    public String buildQuery() throws SQLError {
        if (params.isEmpty())
            return "";

        QueryStringWriter sw = new QueryStringWriter(this)
                .append("INSERT INTO ")
                .append(table)
                .space()
                .append("(")
                .getNames()
                .append(")")
                .lineBreak()
                .append("VALUES")
                .space()
                .append("(")
                .lineBreak()
                .getValues()
                .lineBreak()
                .append(")").lineBreak()
                .append("ON CONFLICT (")
                .append(new Strings()
                        .escaper((Object o) -> Database.escapeSQL(Utils.toString(o)))
                        .addAll(keyColumns).toString(", "))
                .append(") DO ");

        LinkedList<QueryObject> pars = Utils.asList(params);

        for (QueryObject p : params)
            for (String s : keyColumns)
                if (Objects.equals(s, p.name())) {
                    pars.remove(p);
                    break;
                }

        if (pars.isEmpty()) {
            sw.append("NOTHING");
            return sw.toString();
        }

        sw.append("UPDATE SET").lineBreak();

        for (int i = 0; i < pars.size(); i++) {
            QueryObject p = pars.get(i);

            sw.append("\t").append(Database.escapeSQL(p.name()));
            sw.append(" = ");
            sw.append(p.getEscapedValue());
            if (i < pars.size() - 1)
                sw.append(",").lineBreak();
        }

        if (!returning.isEmpty())
            sw.append("\nRETURNING ").append(returning.toString(", "));

        return sw.toString();

    }
}

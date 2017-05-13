package com.database.queries;

import com.utils.collections.Strings;
import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.database.queries.builder.*;
import com.utils.*;
import java.util.LinkedList;
import java.util.List;

public class MergeMultiple extends QueryBuilder<MergeMultiple> {

    private String table;
    private String[] columns;
    List<Object[]> pars = new LinkedList<>();
    private final String keyColumn;

    public MergeMultiple(Database db, String table, String keyColumn, String... columns) {
        super(db);
        this.table = table;
        this.columns = columns;
        this.keyColumn = keyColumn;
    }

    public MergeMultiple add(Object... values) {
        pars.add(values);
        return this;
    }

    @Override
    public String buildQuery() {

        Strings cols = new Strings();
        for (String s : columns)
            cols.add(Database.escapeSQL(s));

        Strings vals = new Strings();

        for (Object[] objs : pars) {
            if (objs == null)
                continue;

            Strings vv = new Strings();
            for (Object o : objs)
                vv.add(db.escape(o));

            vals.add("(" + vv.toString(", ") + ")");

        }

        QueryStringWriter sb = new QueryStringWriter(this);
        sb.append("MERGE INTO ")
                .append(table)
                .space()
                .append("(")
                .append(cols.toString(", "))
                .append(") KEY(")
                .append(keyColumn)
                .append(")\nVALUES\n\t")
                .append(vals.toString(",\n\t"));

        return sb.toString();
    }
}

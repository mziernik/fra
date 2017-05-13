package com.database.queries;

import com.utils.text.StrWriter;
import com.utils.collections.Strings;
import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.exceptions.SQLError;
import com.database.queries.builder.*;
import com.lang.LDatabase;
import com.utils.Undefined;
import java.util.*;

public class InsertMultiple extends QueryBuilder<InsertMultiple> {

    private String table;
    private final QueryColumn[] columns;
    List<Object[]> pars = new LinkedList<>();

    public InsertMultiple(Database db, String table, String... columns) throws SQLError {
        super(db);
        this.table = table;

        if (columns == null || columns.length < 1)
            throw new SQLError(LDatabase.LACK_OF_COLUMN_DEFINITIONS.toString());

        List<QueryColumn> cols = new LinkedList<>();

        for (String s : columns)
            if (s != null)
                cols.add(new QueryColumn(s));

        this.table = table;
        this.columns = cols.toArray(new QueryColumn[cols.size()]);
    }

    private LinkedList<Object> trimUndefinedValues(Object... values) {
        LinkedList<Object> lst = new LinkedList<>();
        if (values != null)
            for (Object o : values)
                if (o != Undefined.TYPE)
                    lst.add(o);
        return lst;
    }

    public InsertMultiple add(Object... values) throws SQLError {
        LinkedList<Object> list = trimUndefinedValues(values);
        if (list.isEmpty())
            return this;
        if (list.size() != columns.length)
            throw new SQLError(LDatabase.INVALID_NUMBER_OF_ARGS.toString(columns.length, list.size()));

        pars.add(list.toArray(new Object[list.size()]));
        return this;
    }

    @Override
    public boolean isEmpty() {
        return pars.isEmpty();
    }

    @Override
    public String buildQuery() {

        if (pars.isEmpty())
            return "";

        Strings cols = new Strings();

        for (QueryColumn c : columns)
            cols.add(c.name);

        Strings vals = new Strings();

        for (Object[] objs : pars) {
            if (objs == null)
                continue;

            Strings vv = new Strings();

            for (int i = 0; i < objs.length; i++)
                vv.add(new QueryObject(db, objs[i])
                        .cast(columns[i].cast)
                        .name(columns[i])
                        .getEscapedValue());

            vals.add("(" + vv.toString(", ") + ")");
        }

        StrWriter sb = new StrWriter();
        sb.append("INSERT INTO ").append(table).append(" (");
        sb.append(cols.toString(", "));
        sb.append(") \nVALUES\n\t").append(vals.toString(",\n\t"));

        return sb.toString();
    }

}

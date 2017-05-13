package com.database.queries;

import com.database.Database;
import com.exceptions.SQLError;
import com.database.queries.builder.*;
import com.lang.LDatabase;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import java.sql.SQLException;
import java.util.*;

public class UpdateMultiple extends QueryBuilder<UpdateMultiple> {

    private final String table;
    private final QueryColumn[] columns;
    List<Object[]> pars = new LinkedList<>();
    private final QueryColumn keyColumn;

    public UpdateMultiple(Database db, String table, String keyColumn, String... columns) throws SQLError {
        super(db);

        if (columns == null || columns.length < 1)
            throw new SQLError(LDatabase.LACK_OF_COLUMN_DEFINITIONS.toString());

        List<QueryColumn> cols = new LinkedList<>();

        for (String s : columns)
            cols.add(new QueryColumn(s));

        this.table = table;
        this.columns = cols.toArray(new QueryColumn[cols.size()]);
        this.keyColumn = new QueryColumn(keyColumn);
    }

    @Override
    public boolean isEmpty() {
        return pars.isEmpty();
    }

    public UpdateMultiple add(Object keyValue, Object... values) throws SQLError {
        if (values == null || values.length == 0)
            return this;

        if (keyValue == null)
            throw new SQLError(LDatabase.EMPYT_PRIMARY_KEY.toString());

        if (values.length != columns.length)
            throw new SQLError(LDatabase.INVALID_NUMBER_OF_ARGS.toString(columns.length, values.length));

        for (Object[] obj : pars)
            if (keyValue.equals(obj[0]))
                throw new SQLError(LDatabase.PRIMARY_KEY_DUPLICATED.toString(Utils.toString(keyValue)));

        List<Object> lst = new LinkedList<>();
        lst.add(keyValue);
        lst.addAll(Arrays.asList(values));
        pars.add(lst.toArray(new Object[lst.size()]));
        return this;
    }

    @Override
    public String buildQuery() {

        QueryStringWriter sb = new QueryStringWriter(this);
        sb.append("UPDATE ").append(table).append(" SET\n");

        for (int i = 0; i < columns.length; i++)
            sb.append("\t")
                    .append(columns[i].name)
                    .append(" = x.")
                    .append(columns[i].name)
                    .append(i < columns.length - 1 ? "," : "")
                    .append("\n");

        sb.append("FROM (values\n");

        Iterator<Object[]> itr = pars.iterator();

        while (itr.hasNext()) {
            Object[] vals = itr.next();
            sb.append("\t(");

            for (int i = 0; i < vals.length; i++)
                sb.append(db.escape(
                        new QueryObject(db, vals[i])
                                .array(true)
                                .name(i == 0 ? keyColumn : columns[i - 1])))
                        .append(i < vals.length - 1 ? ", " : "");

            sb.append(")");
            if (itr.hasNext())
                sb.append(",");
            sb.append("\n");
        }

        Strings cols = new Strings();
        cols.add(keyColumn.name);
        for (QueryColumn c : columns)
            cols.add(c.name);

        sb.append(") AS x (")
                .append(cols.toString(", "))
                .append(")\n")
                .append("WHERE ")
                .append(table)
                .append(".")
                .append(keyColumn.name)
                .append(" = x.")
                .append(keyColumn.name);

        return sb.toString();
    }

    /*
     UPDATE users SET
     user_name = c.user_name,
     user_pass = c.user_pass
     from (values
     ('admin', 'aaaaaa'),
     ('user', 'uuuuu')
     ) AS c(user_name, user_pass)
     WHERE c.user_name = users.user_name;
     */
}

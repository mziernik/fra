package com.database.queries;

import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.queries.builder.QueryObject;
import com.exceptions.SQLError;
import com.database.queries.builder.QueryStringWriter;
import com.lang.LDatabase;
import com.utils.collections.Strings;

public class Merge extends QueryBuilder<Merge> {

    private final String table;
    private final String keyColumn;
    protected final Strings returning = new Strings()
            .trim(true)
            .unique(true)
            .nonEmpty(true);

    public Merge(Database db, String table, String keyColumn) {
        super(db);
        this.table = table;
        this.keyColumn = keyColumn;
    }

    @Override
    public Merge arg(String name, Object value) {
        return super.arg(name, value);
    }

    public Merge addReturningColumn(String column) {
        returning.add(column);
        return this;
    }

    @Override
    public String buildQuery() throws SQLError {
        if (params.isEmpty())
            return "";
        boolean found = false;
        String keyValue = null;
        for (QueryObject obj : params)
            if (keyColumn.equals(obj.name())) {
                found = true;
                keyValue = obj.getEscapedValue();
                break;
            }

        if (!found)
            throw new SQLError(LDatabase.LACK_OF_COLUMN_VALUE.toString(keyColumn));

        /**
         * insert into users.roles(key, name) (select 'aaa', 'bbbb' from
         * users.roles where key != 'aaa');
         *
         */
        if (db instanceof PostgreSQL) {

            QueryStringWriter sb = new QueryStringWriter(this)
                    .append("INSERT INTO ")
                    .append(table)
                    .space()
                    .append("(")
                    .getNames()
                    .append(")(")
                    .lineBreak()
                    .append("SELECT")
                    .lineBreak()
                    .getValues()
                    .lineBreak()
                    .intent()
                    .append("WHERE (SELECT COUNT(*) FROM ").append(table).append(" WHERE ")
                    .append(keyColumn)
                    .append(" = ")
                    .append(keyValue)
                    .append(") = 0")
                    .lineBreak()
                    .append(");")
                    .lineBreak();

            sb.append("UPDATE ").append(table).append(" SET\n");

            boolean first = true;
            for (QueryObject obj : params) {

                if (keyColumn.equals(obj.name()))
                    continue;

                if (!first)
                    sb.append(",");

                sb.append("\t").append(Database.escapeSQL(obj.name()));
                sb.append(" = ");
                sb.append(obj.getEscapedValue());
                sb.append("\n");
                first = false;
            }

            sb.append("WHERE ")
                    .append(keyColumn)
                    .append(" = ")
                    .append(keyValue);

            if (!returning.isEmpty())
                sb.append("\nRETURNING ").append(returning.toString(", "));

            return sb.toString();
        }

        return new QueryStringWriter(this)
                .append("MERGE INTO ")
                .append(table)
                .space()
                .append("(")
                .getNames()
                .append(")")
                .lineBreak()
                .append("KEY(")
                .append(keyColumn)
                .append(")")
                .lineBreak()
                .append("VALUES(")
                .lineBreak()
                .getValues()
                .lineBreak()
                .append(")")
                .toString();
    }
}

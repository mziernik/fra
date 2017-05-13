package com.database.queries;

import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import com.database.queries.builder.QueryBuilder;
import com.database.Database;
import com.exceptions.SQLError;
import com.database.queries.builder.QueryStringWriter;
import com.utils.collections.Strings;

/**
 * W zalezności od tego czy parametr 'where' jest zdefiniowany, wykoany zostanie
 * insert lub update
 *
 * @author milosz
 */
public class InsertOrUpdate extends QueryBuilder<InsertOrUpdate> {

    protected boolean orReplace = false; // SQLite
    protected final String table;
    protected final String where;
    protected final Strings returning = new Strings()
            .trim(true)
            .unique(true)
            .nonEmpty(true);

    public InsertOrUpdate(Database db, String table, String where) {
        super(db);
        this.table = table;
        this.where = where;
    }

    public InsertOrUpdate addReturningColumn(String column) {
        returning.add(column);
        return this;
    }

    @Override
    public InsertOrUpdate arg(String name, Object value) {
        addParam(name, value).array(true);
        return this;
    }

    public InsertOrUpdate argIns(String name, Object value) {
        if (isInsert())
            addParam(name, value).array(true);
        return this;
    }

    public InsertOrUpdate argUpd(String name, Object value) {
        if (isUpdate())
            addParam(name, value).array(true);
        return this;
    }

    public boolean isInsert() {
        return where == null || where.trim().isEmpty();
    }

    public boolean isUpdate() {
        return !isInsert();
    }

    @Override
    public String buildQuery() {
        return isUpdate() ? buildUpdate() : buildInsert();
    }

    protected String buildUpdate() {
        if (params.isEmpty())
            return "";

        StrWriter sb = new StrWriter();
        sb.append("UPDATE ").append(table).append(" SET\n");

        for (int i = 0; i < params.size(); i++) {

            sb.append("\t").append(Database.escapeSQL(params.get(i).name()));
            sb.append(" = ");
            sb.append(params.get(i).getEscapedValue());
            if (i < params.size() - 1)
                sb.append(",");
            sb.append("\n");
        }

        if (!Is.empty(where))
            sb.append("WHERE ")
                    .append(where);

        if (!returning.isEmpty())
            sb.append("\nRETURNING ").append(returning.toString(", "));
        return sb.toString();
    }

    protected String buildInsert() {

        if (params.isEmpty())
            return "";

        QueryStringWriter sw = new QueryStringWriter(this)
                .append("INSERT").append(orReplace ? " OR REPLACE" : "").append(" INTO ")
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
                .append(")");

        if (!returning.isEmpty())
            sw.append("\nRETURNING ").append(returning.toString(", "));

        return sw.toString();
    }

}

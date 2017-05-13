package com.database.queries.builder;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppContext;
import com.database.*;
import com.database.queries.Insert;
import com.exceptions.EError;
import com.json.Escape;
import com.utils.Undefined;
import java.sql.*;
import java.util.*;

@SuppressWarnings("unchecked")
public abstract class QueryBuilder<Builder extends QueryBuilder> implements Iterable<QueryObject> {

    public final QueryOptions options = new QueryOptions(this);
    protected final List<QueryObject> params = new LinkedList<>();
    public QueryExecutor executor;

    public static interface QueryExecutor {

        public QueryRows execute(QueryBuilder qry) throws SQLException;
    }

    public final Database db;

    public QueryBuilder(Database db) {
        this.db = db;
        if (AppContext.devMode)
            options.addNameComments(true);
    }

    @Override
    public String toString() {
        try {
            return buildQuery();
        } catch (SQLException ex) {
            return EError.toString(ex);
        }
    }

    public boolean isEmpty() {
        return params.isEmpty();
    }

    protected abstract String buildQuery() throws SQLException;

    public QueryRows execute() throws SQLException {
        return executor != null ? executor.execute(this) : db.execute(toString());
    }

    protected String getValues() {
        QueryStringWriter sb = new QueryStringWriter(this);
        getValues(sb);
        return sb.toString();
    }

    protected Builder arg(String name, Object value) {
        addParam(name, value);
        return (Builder) this;
    }

    public QueryObject put(String name, Object value) {
        return addParam(name, value);
    }

    protected QueryObject addParam(String name, Object value) {

        QueryObject param = new QueryObject(db, value);

        if (value == Undefined.TYPE)
            return param;

        if (name == null || (value == null && !options.addNulls))
            return param;

        if (!(this instanceof Insert))
            for (QueryObject qp : new LinkedList<>(params))
                if (qp.name.equals(name)) {
                    param = qp;
                    break;
                }

        params.add(param);
        param.name(name);
        param.cast(param.column().cast);
        param.value(value);
        return param;
    }

    protected String getNames() {
        QueryStringWriter sb = new QueryStringWriter(this);
        getNames(sb);
        return sb.toString();
    }

    /**
     * Zwraca parametry w postaci nazwa = "wartosc", nazwa2 = wartosc2
     */
    protected String getParamValueQuery() {
        QueryStringWriter sb = new QueryStringWriter(this);
        for (QueryObject qp : new LinkedList<>(params)) {
            if (!sb.isEmpty())
                sb.append(",").space();
            sb.append(qp.name);
            sb.space().append("=").space();
            sb.append(qp.getEscapedValue());
        }
        return sb.toString();
    }

    protected QueryStringWriter getNames(QueryStringWriter sb) {
        boolean first = true;
        for (QueryObject qp : new LinkedList<>(params)) {
            if (!first)
                sb.append(", ");
            first = false;
            if (options.quotaNames)
                sb.append("\"").append(qp.name).append("\"");
            else
                sb.append(qp.name);
        }
        return sb;
    }

    protected QueryStringWriter getValues(QueryStringWriter sb) {

        String lineBreak = Utils.coalesce(sb.getLineBreak(), "");

        LinkedList<QueryObject> pars = new LinkedList<>(this.params);

        for (int i = 0; i < pars.size(); i++) {

            QueryObject par = pars.get(i);
            sb.append(options.intent);

            if (options.addNameComments && par.name != null && !lineBreak.contains("\n"))
                sb.append("/*").append(par.name).append("*/ ");

            sb.append(par.getEscapedValue());

            if (i < pars.size() - 1)
                sb.append(",");

            if (options.addNameComments && par.name != null && lineBreak.contains("\n"))
                sb.append("\t-- ").append(new Escape().useQuota(false).toString(par.name));

            if (i < pars.size() - 1)

                sb.append(lineBreak);

        }
        return sb;
    }

    @Override
    public Iterator<QueryObject> iterator() {
        return new LinkedList<>(params).iterator();
    }

}

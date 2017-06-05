package com.database;

import com.utils.IUnquoted;
import com.utils.text.StrWriter;
import java.util.*;

public class SqlCondition implements IUnquoted {

    private final Boolean and;
    public boolean compactMode;

    private final LinkedList<SqlCondition> list = new LinkedList<>();
    private final String query;

    private SqlCondition(Boolean operator, String query) {
        this.and = operator;
        this.query = query;
    }

    public SqlCondition(boolean andOperator) {
        this.and = andOperator;
        this.query = null;
    }

    public SqlCondition add(String query) {
        if (query != null && !query.trim().isEmpty())
            list.add(new SqlCondition(null, query.trim()));
        return this;
    }

    public SqlCondition addCondition(boolean andOperator) {
        SqlCondition cond = new SqlCondition(andOperator);
        list.add(cond);
        return cond;
    }

    public void build(StrWriter writer) {
        if (query == null && list.isEmpty())
            return;

        if (query != null || and == null) {
            writer.append(query);
            return;
        }

        if (list.size() == 1) {
            list.peek().build(writer);
            return;
        }

        boolean first = true;

        boolean hasChildren = false;
        for (SqlCondition sc : list)
            hasChildren |= !sc.list.isEmpty();

        if (writer.getLevel() > 0) {
            writer.append("( ");

            if (hasChildren)
                writer.br().intent();
        }

        for (SqlCondition sc : list) {

            //   sb.append(offset).append(intent);
            if (!first)
                if (hasChildren)
                    writer.br().intent();
                else
                    writer.append(" ");

            if (!first) {
                if (writer.isCompact())
                    writer.append(" ");
                writer.append(and ? "AND" : "OR").append(" ");
            }

            writer.nextLevel(() -> {
                sc.build(writer);
            });

            first = false;
        }

        if (writer.getLevel() > 0) {

            if (hasChildren)
                writer.br().intent(writer.getLevel() - 1);
            writer.append(" )");
        }
    }

    @Override
    public String toString() {
        return toString(0);
    }

    public String toString(int level) {
        if (isEmpty())
            return "TRUE";

        StrWriter writer = new StrWriter();
        writer.setLevel(level);
        writer.setLineBreak(compactMode ? " " : "\n");
        build(writer);
        return writer.toString();
    }

    public boolean isEmpty() {
        return (query != null && query.isEmpty()) || list.isEmpty();
    }

}

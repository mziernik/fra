package com.database.queries.builder;

import com.utils.text.StrWriter;
import com.database.Database;

public class QueryStringWriter {

    private final StrWriter sb = new StrWriter();

    private final QueryBuilder builder;

    public QueryStringWriter(QueryBuilder builder) {
        this.builder = builder;
    }

    @Override
    public String toString() {
        return sb.toString();
    }

    public int length() {
        return sb.length();
    }

    public boolean isEmpty() {
        return sb.length() == 0;
    }

    public String getLineBreak() {
        if (builder.options.singleLine)
            return builder.options.intent != null && !builder.options.intent.isEmpty() ? " " : "";
        else
            return builder.options.intent != null && !builder.options.intent.isEmpty() ? "\n" : "";
    }

    public QueryStringWriter lineBreak() {
        sb.append(getLineBreak());
        return this;
    }

    public QueryStringWriter append(String value) {
        sb.append(value);
        return this;
    }

    public QueryStringWriter space() {
        sb.append(builder.options.intent != null && !builder.options.intent.isEmpty()
                ? " "
                : "");
        return this;
    }

    public QueryStringWriter commaLineBreak() {
        if (builder.options.singleLine)
            sb.append(builder.options.intent != null && !builder.options.intent.isEmpty()
                    ? " " : "");
        else
            sb.append(builder.options.intent != null && !builder.options.intent.isEmpty()
                    ? ",\n" : ",");
        return this;
    }

    public QueryStringWriter intent() {
        if (builder.options.intent != null)
            sb.append(builder.options.intent);
        return this;
    }

    public QueryStringWriter getNames() {
        return builder.getNames(this);
    }

    public QueryStringWriter getValues() {
        return builder.getValues(this);
    }

}

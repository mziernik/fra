package com.database.queries.builder;

public class QueryOptions {

    boolean addNulls = true; // jesli false, to parametr rowny null bedzie zignorowany
    boolean addNameComments = false;
    String intent = "\t";
    boolean singleLine = false;
    public String dateFormat = "yyyy-MM-dd HH:mm:ss.SSS";
    boolean quotaNames;

    public QueryOptions addNulls(boolean addNulls) {
        this.addNulls = addNulls;
        return this;
    }

    private final QueryBuilder builder;

    public QueryOptions(QueryBuilder builder) {
        this.builder = builder;
    }

    public QueryOptions quotaNames(boolean quota) {
        this.quotaNames = quota;
        return this;
    }

    public QueryOptions addNameComments(boolean addNameComments) {
        this.addNameComments = addNameComments;
        return this;
    }

    public QueryOptions intent(String intent) {
        this.intent = intent;
        return this;
    }

    public QueryOptions singleLine(boolean singleLine) {
        this.singleLine = singleLine;
        return this;
    }

    public QueryBuilder builder() {
        return builder;
    }

}

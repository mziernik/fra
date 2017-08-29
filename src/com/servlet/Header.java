package com.servlet;

public class Header {

    public final static Header X_REQUESTED_WITH = new Header("X-Requested-With");
    public final static Header X_REQUEST_ID = new Header("X-Request-Id");
    public final static Header X_REQUESTED_EVAL = new Header("X-Requested-Eval", true);
    public final static Header X_REQUESTED_PARAMS = new Header("X-Requested-Params");
    public final static Header CONTENT_TYPE = new Header("Content-Type");
    public final static Header X_REQUESTED_SKIP_LOG = new Header("X-Requested-Skip-Log");
    public final static Header PARENT_REQUEST_ID = new Header("Parent-Request-Id");
    public final static Header ERROR = new Header("Error", true);

    public String name;
    public boolean expose; // czy odpowiedzi na żądanie ajaxa

    public Header(String name) {
        this(name, false);
    }

    public Header(String name, boolean expose) {
        this.name = name;
        this.expose = expose;
    }

}

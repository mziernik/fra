package com.servlet.interfaces;

public enum HttpMethod {

    GET,
    POST,
    HEAD,
    PUT,
    OPTIONS,
    DELETE,
    TRACE,
    CONNECT,
    MOVE,
    PROXY,
    PROPFIND,
    UNKNOWN;

    public static HttpMethod get(String name) {
        for (HttpMethod method : values())
            if (method.name().equalsIgnoreCase(name))
                return method;
        return UNKNOWN;
    }
}

package com.html.core.dict;

/**
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public enum EnctypeType {

    applicationXWWWFormURLEncoded("application/x-www-form-urlencoded"),
    multipartFormData("multipart/form-data"),
    textPlain("text/plain");

    private final String displayName;

    private EnctypeType(String displayName) {
        this.displayName = displayName;

    }

    @Override
    public String toString() {
        return displayName;
    }
}

package com.html.core.dict;

/**
 * @author Miłosz Ziernik
 * @date 26 sierpnia 2015
 * @encoding UTF-8
 */
public enum OlType {

    numbers("1"),
    lower("a"),
    upper("A"),
    romanLower("i"),
    romanUpper("I");

    private String value;

    private OlType(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }

}

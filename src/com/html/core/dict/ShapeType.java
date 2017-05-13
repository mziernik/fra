package com.html.core.dict;

/**
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public enum ShapeType {

    defaultValue("default"),
    rect("rect"),
    circle("circle"),
    poly("poly");

    public final String displayName;

    private ShapeType(String displayName) {
        this.displayName = displayName;
    }

    @Override
    public String toString() {
        return displayName;
    }
}

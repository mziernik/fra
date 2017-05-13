package com.resources.awesome;

/**
 * @author Miłosz Ziernik
 * @date 02 września 2015
 * @encoding UTF-8
 */
public enum FaSize {

    faLg("lg"),
    fa1x("1x"),
    fa2x("2x"),
    fa3x("3x"),
    fa4x("4x"),
    fa5x("5x");

    public final String value;

    FaSize(String value) {
        this.value = value;
    }

}

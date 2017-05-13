package com.html.bootstrap.button;

/**
 * @author Miłosz Ziernik
 * @date 02 listopada 2015
 * @encoding UTF-8
 */
public enum BsButtonSize {

    large("btn-lg"),
    small("btn-sm"),
    normal(null),
    extraSmall("btn-xs"),;

    private String value;

    private BsButtonSize(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }

}

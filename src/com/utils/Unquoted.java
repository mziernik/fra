package com.utils;

/**
 * @author Miłosz Ziernik
 * @date 29 grudnia 2015
 * @encoding UTF-8
 */
public class Unquoted implements IUnquoted {

    private Object obj;

    public Unquoted(Object obj) {
        this.obj = obj;
    }

    @Override
    public String toString() {
        return Utils.toString(obj);
    }

}

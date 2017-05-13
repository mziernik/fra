package com.resources.awesome;

/**
 * @author Miłosz Ziernik
 * @date 02 września 2015
 * @encoding UTF-8
 */
public enum FaRotate {

    r90("90"),
    r180("180"),
    r270("270");

    public final String value;

    FaRotate(String value) {
        this.value = value;
    }

}

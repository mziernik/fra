package com.html.bootstrap.panel;

/**
 * @author Miłosz Ziernik
 * @date 02 listopada 2015
 * @encoding UTF-8
 */
public enum BsPanelType {

    default_,
    primary,
    success,
    info,
    warning,
    danger;

    @Override
    public String toString() {
        return name().replace("_", "");
    }

}

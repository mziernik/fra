package com.html.core.dict;

/**
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public enum SandboxType {

    noValue("sandbox"),
    allow_forms("allow-forms"),
    allow_pointer_lock("allow-pointer-lock"),
    allow_popups("allow-popups"),
    allow_same_origin("allow-same-origin"),
    allow_scripts("allow-scripts"),
    allow_top_navigation("allow-top-navigation");

    private String displayName;

    private SandboxType(String displayName) {
        this.displayName = displayName;

    }

    @Override
    public String toString() {
        return displayName;
    }
}

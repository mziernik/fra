package com.extensions.email;

import com.lang.LExtensions;
import com.lang.core.LString;

public enum EmailType {

    NONE(LExtensions.NONE),
    TO(LExtensions.TO),
    CC(LExtensions.CC),
    BCC(LExtensions.BCC);

    public final LString caption;

    private EmailType(LString caption) {
        this.caption = caption;
    }

    @Override
    public String toString() {
        return caption.toString();
    }

}

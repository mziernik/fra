package com.intf;

import com.lang.LIntf;
import com.lang.core.LString;

//3-stanowy Boolean
public enum BoolAuto {

    TRUE(LIntf.TRUE, Boolean.TRUE),
    FALSE(LIntf.FALSE, Boolean.FALSE),
    AUTO(LIntf.AUTO, null);

    public final LString caption;
    public final Boolean bool;

    private BoolAuto(LString caption, Boolean bool) {
        this.caption = caption;
        this.bool = bool;
    }

}

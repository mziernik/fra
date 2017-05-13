package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RStatus extends RSAdmin {

    protected RStatus() {
        super("status", LUser.STATUS_PREVIEW);
    }

    protected RStatus(String key, LString name) {
        super(key, name);
    }
}

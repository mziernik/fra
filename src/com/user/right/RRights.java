package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RRights extends RRoot {

    protected RRights() {
        super("rights", LUser.RIGHTS_MANAGE);
    }

    protected RRights(String key, LString name) {
        super(key, name);
    }
}

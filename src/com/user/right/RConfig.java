package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RConfig extends RSAdmin {

    protected RConfig() {
        super("config", LUser.CONFIG_MANAGE);
    }

    protected RConfig(String key, LString name) {
        super(key, name);
    }

}

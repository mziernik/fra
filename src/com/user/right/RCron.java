package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RCron extends RSAdmin {

    protected RCron() {
        super("cron", LUser.CRON_MANAGE);
    }

    protected RCron(String key, LString name) {
        super(key, name);
    }

}

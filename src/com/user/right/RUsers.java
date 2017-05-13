package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RUsers extends RSAdmin {

    protected RUsers() {
        super("users", LUser.USERS_MANAGE);
    }

    protected RUsers(String key, LString name) {
        super(key, name);
    }

}

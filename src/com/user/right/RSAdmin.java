package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RSAdmin extends RRoot {

    protected RSAdmin() {
        super("$admin", LUser.ADMIN);
    }

    protected RSAdmin(String key, LString name) {
        super(key, name);
    }

}

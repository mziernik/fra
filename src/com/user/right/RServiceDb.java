package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RServiceDb extends RRoot {

    protected RServiceDb() {
        super("servicedb", LUser.SERVICE_DB_MANAGE);
    }

    protected RServiceDb(String key, LString name) {
        super(key, name);
    }
}

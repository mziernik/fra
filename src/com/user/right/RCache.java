package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RCache extends RRoot {

    protected RCache() {
        super("cache", LUser.CACHE);
    }

    protected RCache(String key, LString name) {
        super(key, name);
    }
}

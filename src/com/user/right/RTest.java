package com.user.right;

import com.lang.LUser;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RTest extends RRoot {

    protected RTest() {
        super("test", LUser.TESTS);
    }

    protected RTest(String key, LString name) {
        super(key, name);
    }

}

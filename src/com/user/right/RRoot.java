package com.user.right;

import com.utils.Utils;
import com.utils.Is;
import com.lang.core.LStr;
import com.lang.core.LString;
import java.util.LinkedHashSet;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class RRoot extends UserRight {

    protected RRoot() {
        super("root", new LStr("ROOT"));
        all.add(this);
    }

    protected RRoot(String key, LString name) {
        super(key, name);
    }

    @Override
    public LinkedHashSet<UserRight> getAll() {
        LinkedHashSet<UserRight> set = new LinkedHashSet<>();
        Utils.visit((UserRight) this, (right, visitor) -> {
            set.add(right);
            for (UserRight r : right)
                visitor.visit(r, visitor);
        });
        return set;
    }

}

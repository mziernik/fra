package com.user;

import com.lang.LController;
import com.lang.LUser;
import com.lang.core.LString;
import java.util.LinkedHashMap;
import java.util.Map;

public class UserGroupType {

    //---------------------------------------------------------------
    //--------------------------------------------------------------
    public final static Map<Character, UserGroupType> all = new LinkedHashMap<>();

    public final static UserGroupType normal = new UserGroupType('N', LUser.USER_GROUP_TYPE__STANDARD);

    public final char key;
    public final LString name;

    public UserGroupType(char key, LString name) {
        this.key = key;
        this.name = name;
        all.put(key, this);
    }

    public void getAll() {

    }

    public static UserGroupType get(Character key) {
        UserGroupType status = all.get(key);
        if (status != null)
            return status;
        throw new RuntimeException(LUser.WRONG_USER_GROUP_TYPE.toString(key));
    }
}

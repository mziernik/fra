package com.user;

import com.lang.LUser;
import com.lang.core.LString;
import java.util.LinkedHashMap;
import java.util.Map;

public class UserType {

    public final static Map<Character, UserType> all = new LinkedHashMap<>();

    public final static UserType normal = new UserType('N', LUser.USER_TYPE__STANDARD);
    public final static UserType api = new UserType('A', LUser.USER_TYPE__API);
    public final static UserType virtual = new UserType('V', LUser.USER_TYPE__VIRTUAL);

    public final char key;
    public final LString name;

    public UserType(char key, LString name) {
        this.key = key;
        this.name = name;
        all.put(key, this);
    }

    public void getAll() {

    }

    @Override
    public String toString() {
        return name.toString();
    }

    public static UserType get(Character key) {
        UserType status = all.get(key);
        if (status != null)
            return status;
        throw new RuntimeException(LUser.WRONG_USER_TYPE.toString(key));
    }

}

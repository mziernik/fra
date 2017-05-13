package com.user;

import com.lang.LUser;
import com.lang.core.LString;
import java.util.LinkedHashMap;
import java.util.Map;

public class UserStatus {

    //-----------------------------------------
    public final static Map<Character, UserStatus> all = new LinkedHashMap<>();

    public final static UserStatus active = new UserStatus('A', LUser.ACTIVE);
    public final static UserStatus disabled = new UserStatus('D', LUser.USER_STATUS__INACTIVE);
    public final static UserStatus removed = new UserStatus('R', LUser.USER_STATUS__REMOVED);
    public final static UserStatus emailVerification = new UserStatus('E', LUser.USER_STATUS__EMAIL_VERIFICATION);
    public final static UserStatus confirmAwaiting = new UserStatus('M', LUser.USER_STATUS__MODERATION);

    public final char key;
    public final LString name;

    public UserStatus(char key, LString name) {
        this.key = key;
        this.name = name;
        all.put(key, this);
    }

    @Override
    public String toString() {
        return name.toString();
    }

    public void getAll() {

    }

    public static UserStatus get(Character key) {
        UserStatus status = all.get(key);
        if (status != null)
            return status;
        throw new RuntimeException("Nieprawidłowy status użytkownika: '" + key + "'");
    }
}

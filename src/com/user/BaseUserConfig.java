package com.user;

import com.exceptions.CoreException;
import com.servlet.Handlers;

/**
 * Miłosz Ziernik 2013/05/13
 */
public class BaseUserConfig {

    public static BaseUserConfig newInstance(BaseUserData user) {
        try {
            return Handlers.userConfig.getConstructor(BaseUserData.class).newInstance(user);
        } catch (Throwable ex) {
            throw new CoreException(ex);
        }
    }

    protected final BaseUserData user;

    public BaseUserConfig(BaseUserData user) {
        this.user = user;
        user.config = this;
    }

    // do przeciążenia
    public void load() throws Exception {
        user.getHandler().loadUserConfig(user);
    }

    // do przeciążenia
    public void save() throws Exception {
        user.getHandler().saveUserConfig(user);
    }
}

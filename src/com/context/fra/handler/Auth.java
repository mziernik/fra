package com.context.fra.handler;

import com.servlet.controller.Controller;
import com.user.BaseAuthHandler;
import com.user.BaseUserData;

public class Auth extends BaseAuthHandler {

    @Override
    public boolean authorize(Controller page, BaseUserData user, boolean authRequest) throws Exception {
        return true;
    }

    @Override
    public boolean checkPageAuthorization(Controller page, boolean force) throws Exception {
        return true;
    }

}

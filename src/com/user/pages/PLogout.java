package com.user.pages;

import com.config.CHttp;
import com.config.CService;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.utils.Url;

@Endpoint(url = {"$logout"}, auth = false, title = "Wylogowanie")
public class PLogout extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        session.user.authorized = false;
        session.logout();

        String redirect = http.params.getStr("redirect", "");
        if (!http.isAjaxRequest)
            redirect(redirect.isEmpty() ? CHttp.url(http.url, "/", null) : new Url(redirect));
        else
            returnPlainText("logouted", 202);
    }

}

package com.webapi;

import com.json.JObject;
import com.json.exceptions.JException;
import com.servlet.Handlers;
import com.servlet.interfaces.Arg;
import com.user.BaseUserData;
import com.user.BaseUsersHandler;
import com.utils.Is;
import com.utils.hashes.Base64;
import com.webapi.core.*;

public class WSession implements WebApi {

    public WSession() {

    }

    @WebApiEndpoint(auth = false)
    public JObject getCurrentSession(WebApiRequest req) throws Exception {
        JObject json = new JObject();

        json.put("id", req.session.id);
        json.put("created", req.session.created.toString(true));
        json.put("remoteAddress", req.session.remoteAddress.getAddress());
        json.put("url", req.session.url.toString());
        json.put("userAgent", req.session.userAgent.toString());

        return json;
    }

    @WebApiEndpoint(auth = false)
    public JObject getCurrentUser(WebApiRequest request) throws Exception {
        BaseUserData user = request.session.user;
        return user != null ? user.getWebApiData() : null;
    }

    @WebApiEndpoint(auth = false, dataType = DataType_old.JSON)
    public JObject authorize(WebApiRequest request,
            @Arg(name = "username") String username) throws JException, Exception {

        String pass = request.getJson().asObject().getStr("pass");
        pass = Base64.decodeStr(pass);

        BaseUserData user = Is.nullR(BaseUsersHandler.instance().getUser(username),
                () -> Handlers.userData.newInstance());
        user.passPlain = pass;
        user.username = username;

        request.session.user = user;

        Handlers.auth.getInstance().authorize(request.http, user, true);

        JObject json = user.getWebApiData();

        json.put("sessionId", request.session.id);

        return json;
    }

}

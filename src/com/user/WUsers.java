package com.user;

import com.json.JArray;
import com.json.JCollection;
import com.json.JObject;
import com.lang.LServlet;
import com.lang.LUser;
import com.servlet.controller.BaseSession;
import com.servlet.interfaces.Arg;
import com.user.right.UserRight;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;
import java.util.List;

public class WUsers implements WebApi {

    @WebApiEndpoint()
    public static JCollection getAll() throws Exception {
        BaseUsersHandler handler = BaseUsersHandler.instance();
        List<BaseUserData> users = handler.getUsers();

        JArray jResp = new JArray();
        jResp.array().addAll("*id", "type", "status", "login", "firstname",
                "lastname", "displayName", "email", "ldap", "created");

        for (BaseUserData user : users) {

            JArray arr = jResp.array();
            arr.add(user.id);
            arr.add(user.type.key);
            arr.add(user.status.key);
            arr.add(user.username);
            arr.add(user.firstname);
            arr.add(user.lastname);
            arr.add(user.displayName);
            arr.add(user.email);
            arr.add(user.ldapAuth);
            arr.add(user.created);
        }

        return jResp;
    }

    @WebApiEndpoint
    public static JObject getUser(@Arg(name = "id", nonEmpty = true) Integer id) {
        BaseUsersHandler handler = BaseUsersHandler.instance();
        BaseUserData user = handler.getUser(id);

        if (user.isRoot())
            throw new Error(LUser.ACCESS_DENIED.toString());

        if (user == null)
            throw new Error(LUser.USER_NOT_FOUND.toString());

        JObject jResp = new JObject();

        jResp.put("id", user.id);
        jResp.put("login", user.username);
        jResp.put("firstname", user.firstname);
        jResp.put("lastname", user.lastname);
        jResp.put("status", user.status.key);
        jResp.put("email", user.email);
        jResp.put("externalauth", (user.ldapAuth == null || user.ldapAuth)); // CLDAP.domain != null && !CLDAP.domain.isNull() && (user.ldapAuth == null || user.ldapAuth));

        // Grupy i uprawnienia
        JObject rights = new JObject();

        for (UserRight r : user.rights.allowed)
            if (!r.equals(UserRight.root))
                rights.add(r.key, true);

        JArray deniedRights = new JArray();
        for (UserRight r : user.rights.denied)
            if (!r.equals(UserRight.root))
                rights.add(r.key, false);

        JArray groups = new JArray();
        for (RightsScheme group : user.rights.groups)
            groups.add(group.key);

        jResp.put("groups", groups);
        jResp.put("rights", rights);

        return jResp;
    }

    @WebApiEndpoint
    public JObject edit(
            @Arg(name = "login", nonEmpty = true) String login
    ) throws Exception {

        return null;
    }

    @WebApiEndpoint()
    public JObject getCurrent(WebApiRequest request) {
        BaseSession session = request.session;

        if (session == null)
            return null;

        BaseUserData user = session.user;

        if (user == null)
            return null;

        if (user.id != null)
            user = BaseUsersHandler.instance().getUser(user.id);

        JObject json = new JObject();
        json.put("sid", session.id);
        json.put("id", user.id);
        json.put("login", user.username);
        json.put("display", user.displayName);
        json.put("firstname", user.firstname);
        json.put("lastname", user.lastname);
        json.put("email", user.email);

        return json;
    }

    @WebApiEndpoint()
    public static JObject getUserTypes() {
        JObject jResp = new JObject();
        for (UserType ut : UserType.all.values())
            jResp.put(Character.toString(ut.key), ut.name);
        return jResp;
    }

    @WebApiEndpoint
    public static JObject getUserStatuses() {
        JObject jResp = new JObject();
        for (UserStatus us : UserStatus.all.values())
            jResp.put(Character.toString(us.key), us.name);
        return jResp;
    }

    @WebApiEndpoint
    public JObject getRoles() {
        return null;
    }

    @WebApiEndpoint
    public JObject getRoleGroups() {
        return null;
    }

    @WebApiEndpoint
    public JObject getUserGroups() {
        return null;
    }

    @WebApiEndpoint
    public JObject getTypes() {
        return null;
    }

    @WebApiEndpoint
    public JObject getStatuses() {
        return null;
    }

}

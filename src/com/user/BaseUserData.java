package com.user;

import com.utils.Utils;
import com.utils.Is;
import com.events.ServiceEvent;
import com.exceptions.ThrowableException;
import com.json.JObject;
import com.servlet.Handlers;
import com.servlet.controller.BaseSession;
import com.user.right.UserRights;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.utils.reflections.Revertable;
import com.lang.LUser;
import java.util.*;

/**
 * Miłosz Ziernik 2013/03/10
 */
public class BaseUserData implements Revertable<BaseUserData> {

    public UserStatus status = UserStatus.disabled;
    public UserType type = UserType.normal;
    public String username = "";
    @Deprecated
    public String displayName = "";
    public Integer id;
    public String passPlain;
    public String passHash;
    public boolean exists = false;
    public String token;
    public TDate created;
    public boolean authorized = false;
    public final UserRights rights = new UserRights(this);
    public final Map<String, String> attributes = new LinkedHashMap<>();
    BaseUserConfig config;
    public Boolean ldapAuth; // wymuszenie autoryzacji LDAP
    public String firstname;
    public String lastname;
    public String email;
    public Date passwordExpire;
    public String createdBy;
    private final BaseUsersHandler handler;
    public final Map<String, String> ldapAttributes = new LinkedHashMap<>();
    public boolean autoLogin; // użytkownik zalogowany automatycznie na podstawie adresu ip z pominięciem autoryzacji

    public static BaseUserData newInstance(BaseUsersHandler handler) {
        try {
            return Handlers.userData.getHandlerClass()
                    .getDeclaredConstructor(BaseUsersHandler.class)
                    .newInstance(handler);
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    public static BaseUserData newInstance() {
        return Handlers.userData.getInstance((Object) null);
    }

    // do przeciazenia
    public BaseUserConfig config() {
        return config != null ? config : BaseUserConfig.newInstance(this);
    }

    public BaseUserData(BaseUsersHandler handler) {
        this.handler = handler;
    }

    @Override
    public String toString() {
        return username == null ? LUser.LACK.toString() : (username + ", " + LUser.AUTHORIZATION.toString() + ": "
                + Utils.boolToStr(authorized)
                + (!rights.groups.isEmpty() ? ", " + LUser.GROUPS.toString() + ": " + new Strings(rights.groups)
                : ""));

    }

    public void authorize(String plainPassword) {
        authorized = false;
        if (plainPassword == null)
            return;
        BaseUsersHandler handler = isRoot() ? new LocalUsersHandler()
                : getHandler();
        authorized = handler.getHash(plainPassword).equalsIgnoreCase(passHash);
    }

    public BaseUsersHandler getHandler() {
        return handler != null ? handler : BaseUsersHandler.instance();
    }

    public List<BaseUsersHandler.UserEvent> getEvents() throws Exception {
        return getHandler().getUserEvents(username);
    }

    public boolean isRoot() {
        return username != null && username.equalsIgnoreCase("root");
    }

    public ServiceEvent event(Object value) {
        return new ServiceEvent(LUser.USER.toString(), Utils.toString(value));

    }

    public static String getCurrentUserName() {
        BaseUserData user = getCurrentUser();
        return user != null ? user.username : null;
    }

    public static BaseUserData getCurrentUser() {
        BaseSession session = BaseSession.getInstance();
        return session != null && session.user != null ? session.user : null;
    }

    public void update() throws Exception {
        getHandler().editUser(this, false);
    }

    public String getFullName() {
        Strings strs = new Strings().nonEmpty(true);
        strs.add(firstname);
        strs.add(lastname);

        if (!strs.isEmpty())
            return strs.toString(" ");
        else
            return username;
    }

    public String getDisplayValue() {
        if (Is.empty(firstname) && Is.empty(lastname))
            return username;
        if (!Is.empty(firstname) && !Is.empty(lastname))
            return Character.toUpperCase(firstname.charAt(0)) + ". " + lastname;
        return !Is.empty(firstname) ? firstname : lastname;
    }

    public JObject getWebApiData() {
        JObject json = new JObject();
        json.put("username", username);
        json.put("firstname", firstname);
        json.put("lastname", lastname);
        json.put("email", email);
        json.put("type", type.key);
        json.put("status", status.key);
        return json;
    }
}

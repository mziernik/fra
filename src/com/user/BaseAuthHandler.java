package com.user;

import com.servlet.controller.BaseSession;
import com.utils.Utils;
import com.utils.Is;
import com.config.CAuthorization;
import com.exceptions.http.Http403ForbiddenException;
import com.utils.hashes.Hashes;
import com.utils.hashes.Base64;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.user.pages.PPasswordChange;
import com.config.CAuthorization.CLDAP;
import com.context.AppConfig;
import com.google.gson.*;
import com.database.service.Sessions;
import com.events.ServiceEvent;
import com.exceptions.*;
import com.lang.LUser;
import com.servlet.Handlers;
import com.servlet.controller.*;
import com.servlet.requests.HttpRequest;
import com.user.pages.PLogin;
import com.utils.Url;
import com.utils.collections.Strings;

import com.utils.collections.Pair;

/**
 * Miłosz Ziernik 2013/03/10
 */
public class BaseAuthHandler {

    public static BaseAuthHandler newInstance() {
        return Handlers.auth.getInstance();
    }

    //public final static String CUnauthorized = "Nieprawidłowa nazwa użytkownika lub hasło";
    // public final static String CForbidden = "Brak dostępu";
    // public final static String CInactive = "Użytkownik nieaktywny";
    /*
     private static BaseAuthHandler instance;

     public static BaseAuthHandler getInstance() {
     if (instance == null
     || !(instance.getClass() != AppContext.index.handlers.authHandler))
     try {
     instance = AppContext.index.handlers.authHandler.newInstance();
     } catch (Exception ex) {
     throw new CoreException(ex);
     }
     return instance;
     }
     */
    // =========================== do przeciążenia =============================
    protected void onAuthorized(JsonObject jUser) throws Exception {
        // metoda wywoływana po autoryzacji użytkownika przez plik roamingu,
        // przed wczytaniem konfiguracji
    }

    protected void checkAuthorization(BaseUserData user) throws Exception {

    }

    //------------------
    protected boolean authorizedByRoamingFile = false;
    //---------------------------


    /*
     // ukryty konstruktor
     protected BaseUserData() {
     Thread.currentThread().getStackTrace();
     if (BaseContext.resources.hUserConfig != null) {
     try {
     config = BaseContext.resources.hUserConfig.newInstance();
     config._setUserData(this);
     } catch (Exception ex) {
     Log.warning(ex);
     }
     }
     }
     */
    public static boolean rememberUserLoginAfterLogout = true;

    private static boolean rootChecked = false;

    protected boolean checkRootExists(Controller page) throws Exception {
        if (rootChecked || !AppConfig.rootUserMustExists)
            return true;

        HttpRequest http = page.http();
        BaseUserData root = BaseUsersHandler.instance().getUser("root");

        if (root != null && root.passHash != null && !root.passHash.isEmpty()) {
            rootChecked = true;
            return true;
        }

        if (page instanceof PPasswordChange)
            return true;

        http.redirect(new Url(PPasswordChange.class)
                .param("user", "root")
                .param("redirect", page.http().url));

        return false;
    }

    public boolean checkPageAuthorization(Controller page, boolean force) throws Exception {

        if (!checkRootExists(page))
            return false;

        BaseSession session = BaseSession.getInstance();

        //------------------------------ autoryzacja -----------------------
        if ((force || page.endpoint().auth())
                && (!session.user.authorized || session.user.status != UserStatus.active))
            if (!authorize(page, session.user, false))
                return false;

        if (!session.user.rights.has(page.getClass()))
            throw new Http403ForbiddenException();

        return true;
    }

    public boolean authorize(Controller page, BaseUserData user, boolean authRequest)
            throws Exception {
        try {

            HttpRequest http = page.http();
            if (authRequest) {
                user.username = http.params.getStr("username", "");
                user.passPlain = http.params.getStr("password", "");
            }

            String sHtAuth = http.request.getHeader("Authorization");
            if (!authRequest && sHtAuth != null && sHtAuth.toLowerCase().startsWith("basic ")) {

                sHtAuth = sHtAuth.substring("basic ".length());
                try {
                    sHtAuth = new String(Base64.decode(sHtAuth));
                    if (sHtAuth.contains(":")) {
                        user.username = sHtAuth.substring(0, sHtAuth.indexOf(":"));
                        user.passPlain = sHtAuth.substring(sHtAuth.indexOf(":") + 1);
                    }
                } catch (Exception e) {
                    Log.warning(e);
                }
            }

            boolean result = authorize(http, user, authRequest);

            if (!result && !authRequest) {
                Log.debug("Wymagana autoryzacja kontrolera " + page.getClass().getName());
                PLogin.redirect(page);
            }

            return result;
        } catch (Throwable e) {
            PLogin.redirect(page);
            throw e;
        }

    }

    public boolean authorize(HttpRequest http, BaseUserData user, boolean authRequest)
            throws Exception {

        if (http != null)
            Sessions.getSessionData(http.session, http);

        user.authorized = false;
        if (user.username != null)
            user.username = user.username.toLowerCase().trim();

        if (http != null) {
            String host = http.request.getRemoteHost();

            for (Pair<String, String> pair : CAuthorization.autoLogin.value())
                if (host.equals(pair.first)) {
                    user.username = pair.second;
                    user.autoLogin = true;
                    user.authorized = true;
                }
        }

        if (!Is.empty(user.username))
            new Log(LogKind.DEBUG).
                    tag("Authorization")
                    .value(user.username)
                    .attribute("passMD5", user.passPlain != null ? Hashes.md5(user.passPlain) : user.passHash)
                    .send();

        if (user.id == null
                && !user.autoLogin
                && (Is.empty(user.username)
                || Is.empty(user.passPlain)))

            return false;

        boolean save = false;
        user.authorized = false;
        user.status = null;
        user.attributes.clear();

        BaseUsersHandler users = BaseUsersHandler.instance();

        try {

            //   String prevPass = user.passHash;
            boolean exists = users.getUser(user, true);
            // user.authorized = (user.isRoot() && Utils.coalesce(prevPass, "").equalsIgnoreCase(user.passHash));

            user.authorize(user.passPlain);

            if (user.authorized && !Is.empty(user.passPlain))
                user.passHash = users.getHash(user.passPlain);

            if (user.authorized && user.isRoot())
                user.status = UserStatus.active;

            if (!user.isRoot()
                    && !user.autoLogin
                    && !Boolean.FALSE.equals(user.ldapAuth)
                    && users.features.ldapAuth
                    && CLDAP.enabled.value()) {

                BaseLdap ldap = Handlers.ldap.getConstructor(String.class, String.class, String.class, String.class)
                        .newInstance(null, null, user.username, user.passPlain);

                try {
                    user.ldapAttributes.putAll(ldap.getAttributes(user.username));
                    /* save |= !user.passHash.equals(user.ldapPass);
                     user.ldapPass = user.passHash; */

                    // jesli uzytkownik zostal zautoryzowany przez ldapa a nie istnieje w bazie
                    if (!exists)
                        throw new AccessDeniedException();

                    user.authorized = true;

                } catch (javax.naming.AuthenticationException e) {
                    throw new AuthenticationException(LUser.INCORRECT_USERNAME_OR_PASSWORD.toString())
                            .details("LDAP Message", e.getMessage())
                            .details("LDAP", ldap.toString());
                }
            }

            user.authorized |= user.autoLogin;

            if (!exists)
                throw new AuthenticationException(LUser.INCORRECT_USERNAME_OR_PASSWORD.toString());

            newInstance().checkAuthorization(user);

            if (!user.authorized)
                throw new AuthenticationException(LUser.INCORRECT_USERNAME_OR_PASSWORD.toString());

            if (user.status == UserStatus.active && !user.rights.groups.isEmpty()) {
                user.status = null;
                for (RightsScheme group : user.rights.groups)
                    if (group.enabled)
                        user.status = UserStatus.active;
            }

            if (user.status != UserStatus.active)
                throw new ForbiddenError(LUser.USER_NOT_ACTIVE.toString());

            if (authRequest) {
                ServiceEvent evt = user.event("Autoryzacja").tag("authorize")
                        .attribute("user|login", "login", user.username)
                        .key("user_id", user.id);

                HttpRequest req = HttpRequest.getInstance();
                if (req != null)
                    evt.attribute("user|address", "Adres", req.request.getRemoteAddr())
                            .attribute("user|ua", "User-Agent", req.userAgent.getShortUA());

                evt.execute();
            }

            if (http != null)
                Sessions.updateSession(http.session, http);

            Log.event("Authorization", user.username
                    + " authorized" + (user.rights.groups.isEmpty() ? ""
                    : ", groups: " + new Strings(user.rights.groups)));

            if (http != null)
                http.session.loadConfig();

            if (save)
                user.update();

            return true;
        } catch (Throwable e) {
            user.authorized = false;
            Log.warning(e);
            if (http != null)
                PLogin.setLoginError(http.session, e);
            throw e;
        }
    }

}

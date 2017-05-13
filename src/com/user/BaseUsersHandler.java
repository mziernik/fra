package com.user;

import com.utils.Utils;
import com.utils.Is;
import com.utils.hashes.Hashes;
import com.mlogger.Log;
import com.google.gson.GsonBuilder;
import com.json.JObject;
import com.json.JSON;
import com.cache.CachedData;
import com.events.ServiceEvent;
import com.exceptions.ServiceError;
import com.html.core.tag.form.Form;
import com.html.core.tag.table.Tr;
import com.lang.LUser;
import com.servlet.Handlers;
import com.servlet.requests.HttpRequest;
import com.utils.collections.Strings;
import java.lang.reflect.*;
import java.util.*;
import java.util.Date;

import com.user.right.UserRight;

/**
 * Miłosz Ziernik 2013/03/22
 */
public abstract class BaseUsersHandler {

    public final UserHandlerFeatures features = new UserHandlerFeatures();

    protected final List<BaseUserData> allUsers = new LinkedList<>();
    private boolean usersLoaded = false;

    //*************************************************************************
    protected boolean initialized = false;
    public static int maxNameLength = 50;

    /**
     * Przeładowanie struktury grup użytkowników, oraz uprawnień
     *
     * @throws Exception
     */
    public abstract void reload() throws Exception;

    public BaseUserData getUser(String username) {

        if (username == null)
            return null;

        username = username.trim().toLowerCase();
        synchronized (allUsers) {
            for (BaseUserData user : allUsers)
                if (username.equalsIgnoreCase(user.username)
                        || username.equalsIgnoreCase(user.email))
                    return user;
        }
        return null;
    }

    public BaseUserData getUserF(Integer id) {
        BaseUserData user = getUser(id);
        if (user == null)
            throw new ServiceError(LUser.USER_NOT_FOUND.toString()).details("user_id", id);
        return user;
    }

    public BaseUserData getUser(Integer id) {
        if (id != null)
            synchronized (allUsers) {
                for (BaseUserData user : allUsers)
                    if (user.id != null && user.id == id)
                        return user;
            }
        return null;
    }

    protected abstract void doInitialize() throws Exception;

    // metoda wywołana po zainicjowaniu kontekstu
    public void initialize() throws Exception {
        if (initialized)
            return;

        doInitialize();
        reload();
        initialized = true;
    }

    public static LocalUsersHandler local() {
        return (LocalUsersHandler) Handlers.users.defaultInstance;
    }

    public static BaseUsersHandler instance() {
        return Handlers.users.getInstance();
    }

    public BaseUserData newUserData() {
        return BaseUserData.newInstance(this);
    }

    public List<BaseUserData> getUsers() throws Exception {
        synchronized (allUsers) {
            return new LinkedList<>(allUsers);
        }
    }

    public <T extends BaseUserData> List<T> getUsers(Class<T> userData) throws Exception {
        synchronized (allUsers) {
            List<T> list = new LinkedList<>();
            list.addAll((Collection<? extends T>) allUsers);
            return list;
        }
    }

    protected abstract boolean doGetUser(BaseUserData user, boolean includeAttributes) throws Exception;

    public boolean getUser(BaseUserData user, boolean includeAttributes) throws Exception {
        if (user == null || (user.username == null && user.id == null))
            return false;
        return doGetUser(user, includeAttributes);
    }

    protected abstract void doEditUser(BaseUserData user, boolean isNew) throws Exception;

    public void editUser(BaseUserData user, boolean isNew) throws Exception {
        //TODO: powiązać z authorizeWithAlternatuveLogins, authorizeWithEmail

        if (user.isRoot()) {
            user.status = UserStatus.active;
            user.ldapAuth = false;
            user.rights.allowed.clear();
            user.rights.denied.clear();
            user.rights.allow(UserRight.root);
        }

        if (user.passHash == null && user.passPlain != null)
            user.passHash = getHash(user.passPlain);

        if (Is.empty(user.token))
            user.token = UUID.randomUUID().toString();

        if (isNew)
            synchronized (allUsers) {
                for (BaseUserData usr : allUsers)
                    if (usr.username.equalsIgnoreCase(user.username)
                            || (usr.id != null && usr.id.equals(user.id)))
                        throw new Error(LUser.USER_ALREADY_EXIST.toString(user.username));
            }

        doEditUser(user, isNew);

        if (isNew)
            allUsers.add(user);

        new ServiceEvent(this.getClass().getSimpleName(), (isNew ? LUser.REGISTRATION.toString() : LUser.UPDATE.toString())
                + " " + LUser.USER_GENITIVE + " \"" + user.username + "\"")
                .key("int_user_id", user.id)
                .key("ext_user_id", user.id)
                .attribute("firstname", "Imię", user.firstname)
                .attribute("lastname", "nazwisko", user.lastname)
                .attribute("email", "E-mail", user.email)
                .attribute("disp_name", "Nazwa", user.displayName)
                .execute();

    }

    protected abstract void doRemoveUser(BaseUserData user) throws Exception;

    public void removeUser(BaseUserData user) throws Exception {

        if (user.isRoot())
            throw new UnsupportedOperationException();

        doRemoveUser(user);

        synchronized (allUsers) {
            for (BaseUserData usr : allUsers)
                if (usr.username.equals(user.username)) {
                    allUsers.remove(usr);
                    break;
                }
        }

    }

    public List<UserEvent> getUserEvents(String username) {
        return null;
    }

    public void checkName(String name) throws Error {

        final String chrs = "+-_!@#$%^&().,~'`=[]{};";

        if (name == null || name.trim().isEmpty())
            throw new Error(LUser.NAME_CANT_BE_EMPTY.toString(name));

        if (name.length() > maxNameLength)
            throw new Error(LUser.NAME_TO_LONG.toString(name, maxNameLength));

        if (name.contains(" "))
            throw new Error(LUser.NAME_CANT_CONTAINS_SPACES.toString(name));

        Set<String> wrong = new HashSet<>();
        for (char c : name.toCharArray())
            if (!((c >= '0' && c <= '9')
                    || (c >= 'A' && c <= 'Z')
                    || (c >= 'a' && c <= 'z')
                    || chrs.contains(Character.toString(c))))
                wrong.add("\"" + c + "\"");

        if (!wrong.isEmpty())
            throw new Error(LUser.INCORRECT_CHARS_IN_NAME.toString(name,
                    new Strings(wrong).toString(", ")));
    }

    /**
     * Domyślna metoda hashowania
     */
    public String getHash(String text) {
        return Hashes.hash(Hashes.Hash.MD5, text);
    }

    public void verifyPassword(String password) {
        if (password == null || password.trim().isEmpty())
            throw new Error(LUser.PASSWORD_CANT_BE_EMPTY.toString());
        if (password.length() < 4)
            throw new Error(LUser.PASSWORD_CANT_BE_LESS_THAN.toString(4));
    }

    //*****************************************************************************
    //
    //
    //
    //
    //*****************************************************************************
    public abstract void doEditGroup(RightsScheme group, boolean isNew) throws Exception;

    public void editGroup(RightsScheme data, boolean isNew) throws Exception {
        if (data == null)
            return;

        checkName(data.key);
        doEditGroup(data, isNew);
        if (isNew)
            RightsScheme.add(data);

        Log.event("user", LUser.USER_GROUP_UPDATED.toString(data.key));
    }

    protected abstract void doRemoveGroup(RightsScheme group) throws Exception;

    public void removeGroup(RightsScheme group) throws Exception {
        doRemoveGroup(group);
        RightsScheme.remove(group);
        Log.event("user", LUser.USER_GROUP_REMOVED.toString(group.name));
    }

    //*****************************************************************************
    //
    //
    //
    //
    //*****************************************************************************
    protected abstract JObject doLoadUserConfig(BaseUserData user) throws Exception;

    public boolean loadUserConfig(BaseUserData user) throws Exception {
        if (user == null)
            return false;
        try {
            JObject cfg = doLoadUserConfig(user);
            if (cfg == null)
                return false;
            cfg.deserialize(user.config());
            return true;
        } catch (Exception e) {
            Log.warning(e);
        }
        return false;
    }

    protected abstract void doSaveUserConfig(BaseUserData user, JObject data) throws Exception;

    public void saveUserConfig(BaseUserData user) throws Exception {
        JObject json = JSON.serialize(user.config(), getBuilder()).asObject();
        doSaveUserConfig(user, json);
    }

    protected GsonBuilder getBuilder() {
        GsonBuilder builder = new GsonBuilder();
        builder.setPrettyPrinting();
        builder.setDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        builder.excludeFieldsWithModifiers(
                Modifier.PRIVATE,
                Modifier.STATIC,
                Modifier.PROTECTED);
        return builder;
    }

    /**
     *
     *
     * Obsługa wyświetlania i edycji
     *
     *
     */
    public String[] getUsersPageColumnsHeaders() {
        return new String[]{LUser.NAME.toString()};
    }

    public boolean displayUserData(Tr row, BaseUserData user) {
        row.setCells(user.displayName);
        // double dd = (double) user.getAllRoles().size() / (double) Role.allRoles.size();
        // row.td().text(Math.round(dd * 100) + "%");
        return true;
    }

    public void editUserPage(BaseUserData user, Form frm) {

    }

    public void exportGroups(HttpRequest request) {

    }

    public void importGroups(CachedData file, Boolean override) {

    }

    public void importUsers(CachedData file, Boolean override) {

    }

    public abstract void doExportUsers(HttpRequest request) throws Exception;

    public void exportUsers(HttpRequest request) throws Exception {
        doExportUsers(request);
    }

    public class UserHandlerFeatures {

        public boolean passwordChange = true;
        public boolean editUser = true;
        public boolean removeUser = true;
        public boolean displayGroups = true;
        public boolean addGroup = true;
        public boolean editGroup = true;
        public boolean removeGroup = true;
        public boolean authorizeWithAlternatuveLogins = true;
        public boolean authorizeWithEmail = true;
        public boolean ldapAuth = true; // uzyj ldapa, jesli jest dostepny
    }

    public static class UserEvent {

        public Date date = new Date();
        public String name;
        public String value;
        public String details;

    }

}

package com.user;

import com.utils.Utils;
import com.utils.Is;
import com.database.*;
import com.database.queries.*;
import com.database.service.ServiceDB;
import com.exceptions.SQLError;
import com.json.*;
import com.servlet.requests.HttpRequest;
import com.user.right.UserRight;
import com.utils.Unquoted;
import com.utils.collections.Strings;
import java.sql.SQLException;
import java.util.*;

public class LocalUsersHandler extends BaseUsersHandler {

    public LocalUsersHandler() {

    }

    @Override
    protected void doInitialize() throws Exception {
        ServiceDB db = new ServiceDB();
        MultipleQuery mqry = db.multipleQuery();

        //FixMe: Dorobić obsługę typów użytkowników oraz grup
        Strings rights = new Strings();
        Strings groups = new Strings();

        // aktualizacja uprawnień
        for (UserRight right : UserRight.root.getAll()) {
            mqry.add(db.merge("users.rights", "key")
                    .arg("key", right.key)
                    .arg("name", right.name)
                    .arg("parent_right", right.parent != null ? right.parent.key : null));
            rights.add(right.key);
        }
        // aktualziacja grup
        for (RightsScheme group : RightsScheme.getAll()) {
            mqry.add(db.merge("users.groups", "key")
                    .arg("key", group.key)
                    .arg("name", group.name)
                    .arg("description", group.details)
                    .arg("embedded", group.embedded));

            groups.add(group.key);
        }

        if (!rights.isEmpty())
            mqry.query("DELETE FROM users.rights WHERE NOT key IN (?)", rights);

        if (!groups.isEmpty())
            mqry.query("DELETE FROM users.groups WHERE NOT key IN (?)", groups);

        mqry.execute();
    }

    @Override
    public void reload() throws Exception {
        ServiceDB db = new ServiceDB();

        RightsScheme.clearAll();

        for (QueryRow row : db.execute("SELECT g.*, \n"
                + "    (   \n"
                + "        SELECT group_concat(right_key || ':' || refused)\n"
                + "        FROM users.group_rights gr\n"
                + "        WHERE gr.group_key = g.key\n"
                + "    ) as rights\n"
                + "FROM users.groups g")) {
            RightsScheme group = null;

            if (row.getBool("embedded")) {
                group = RightsScheme.get(row.getStr("key"), true);
                if (group != null && !group.embedded)
                    throw new UnsupportedOperationException(group.key + " embedded");
            }

            if (group == null)
                group = new RightsScheme(row.getStr("key"), row.getStr("name"));

            group.details = row.getStr("description", null);
            group.enabled = row.getBool("enabled");

            for (String ss : row.getStr("rights", "").split(",")) {
                String[] arr = ss.split(":");
                if (arr.length != 2)
                    continue;
                Boolean refused = Utils.strBool(arr[1], false);
                UserRight right = UserRight.get(arr[0]);
                if (right == null)
                    continue;
                if (refused)
                    group.refused.add(right);
                else
                    group.allowed.add(right);
            }
        }
    }

//    @Override
//    protected void doGetUsers(List<BaseUserData> list) throws Exception {
//        for (QueryRow row : new ServiceDB().execute(USERS_QUERY, true)) {
//            BaseUserData user = BaseUserData.newInstance(this);
//            processUserRow(user, row);
//            list.add(user);
//        }
//    }
    private void processUserRow(BaseUserData user,
            QueryRow row) throws SQLException {

        user.id = row.getInt("user_id");
        user.status = UserStatus.get(row.getStr("status").charAt(0));
        user.type = UserType.get(row.getStr("type").charAt(0));
        user.username = row.getStr("username");
        user.displayName = row.getStr("display_name");
        user.passHash = row.getStr("password", null);
        user.firstname = row.getStr("first_name", null);
        user.lastname = row.getStr("last_name", null);
        user.email = row.getStr("email", null);
        user.ldapAuth = row.getBool("ldap_auth");
        user.token = row.getStr("external_token");
        user.createdBy = row.getStr("created_by", null);

        for (String ss : row.getStr("rights", "").split(",")) {
            String[] arr = ss.split(":");
            if (arr.length != 2)
                continue;
            Boolean refused = Utils.strBool(arr[1], false);
            UserRight right = UserRight.get(arr[0]);
            if (right == null)
                continue;

            if (refused)
                user.rights.denny(right);
            else
                user.rights.allow(right);
        }

        for (String s : row.getStr("groups", "").split(","))
            if (!s.isEmpty())
                user.rights.addGroup(RightsScheme.get(s, true));

    }

    @Override
    protected boolean doGetUser(BaseUserData user, boolean includeAttributes) throws Exception {
        ServiceDB db = new ServiceDB();
        QueryRow row = db.execute(USERS_QUERY,
                new Unquoted("username = " + db.escape(user.username)))
                .first();

        if (row == null)
            return false;

        processUserRow(user, row);

        return true;
    }

    @Override
    protected synchronized void doEditUser(BaseUserData user, boolean isNew) throws Exception {

        if (isNew) {
            if (Is.empty(user.passPlain))
                user.passPlain = Utils.randomId(6);
            user.passHash = getHash(user.passPlain);
        }

        new ServiceDB().transaction((Database _db) -> {
            ServiceDB db = (ServiceDB) _db;

            InsertOrUpdate qry = db.insertOrUpdate("users.users",
                    user.id != null ? "user_id = " + user.id : null);

            if (user.username != null && user.username.equalsIgnoreCase("ROOT")) {
                user.rights.allow(UserRight.root);
                user.status = UserStatus.active;
            }

            if (user.id != null)
                qry.arg("user_id", user.id);

            if (Is.empty(user.token))
                user.token = UUID.randomUUID().toString();

            if (user.displayName == null)
                user.displayName = new Strings(user.firstname, user.lastname).toString(" ");

            if (user.displayName.isEmpty())
                user.displayName = user.username;

            qry.arg("username", user.username)
                    .arg("status", user.status != null
                            ? user.status.key
                            : UserStatus.disabled.key)
                    .arg("type", user.type != null
                            ? user.type.key
                            : UserType.normal.key)
                    .arg("created_by", user.createdBy)
                    .arg("external_token", user.token)
                    .arg("password", user.passHash)
                    .arg("first_name", user.firstname)
                    .arg("last_name", user.lastname)
                    .arg("display_name", user.displayName)
                    .arg("email", user.email);

            QueryRow row = qry.execute().generatedKeys.first();
            user.id = row != null ? row.getInt(0) : user.id;

            db.execute("DELETE FROM users.user_rights WHERE user_id = ?", user.id);
            db.execute("DELETE FROM users.user_groups WHERE user_id = ?", user.id);

            for (RightsScheme group : user.rights.groups)
                db.insert("users.user_groups")
                        .arg("user_id", user.id)
                        .arg("group_key", group.key)
                        .execute();

            for (UserRight r : user.rights.allowed)
                db.insert("users.user_rights")
                        .arg("user_id", user.id)
                        .arg("right_key", r.key)
                        .arg("refused", false)
                        .execute();

            for (UserRight r : user.rights.denied)
                db.insert("users.user_rights")
                        .arg("user_id", user.id)
                        .arg("right_key", r.key)
                        .arg("refused", true)
                        .execute();
        });

    }

    @Override
    protected void doRemoveUser(BaseUserData user) throws Exception {
        if (user.isRoot() || user.id == null)
            throw new UnsupportedOperationException();

        MultipleQuery mqry = new ServiceDB().multipleQuery();
        mqry.query("DELETE FROM users.user_rights WHERE user_id = ?", user.id);
        mqry.query("DELETE FROM users.user_groups WHERE user_id = ?", user.id);
        mqry.query("DELETE FROM users.users WHERE user_id = ?", user.id);
        mqry.execute();
    }

    @Override
    public void doEditGroup(RightsScheme group, boolean isNew) throws SQLError, SQLException {

        new ServiceDB().transaction((Database db) -> {
            QueryRow row = db.insertOrUpdate("users.groups",
                    isNew ? null : "key = ?", group.key)
                    .arg("key", group.key)
                    .arg("name", group.name)
                    .arg("description", group.details)
                    .arg("embedded", group.embedded)
                    .arg("enabled", group.enabled)
                    .execute().generatedKeys
                    .first();

            //       db.execute("SELECT * FROM users.groups");
            db.execute("DELETE FROM users.group_rights WHERE group_key = ?", group.key);

            for (UserRight r : group.allowed)
                db.insert("users.group_rights")
                        .arg("group_key", group.key)
                        .arg("right_key", r.key)
                        .arg("refused", false)
                        .execute();

            for (UserRight r : group.refused)
                db.insert("users.group_rights")
                        .arg("group_key", group.key)
                        .arg("right_key", r.key)
                        .arg("refused", true)
                        .execute();
        });

    }

    @Override
    protected void doRemoveGroup(RightsScheme group) throws SQLException {

        MultipleQuery mqry = new ServiceDB().multipleQuery();
        mqry.query("DELETE FROM users.group_rights WHERE group_key = ?", group.key);
        mqry.query("DELETE FROM users.user_groups WHERE group_key = ?", group.key);
        mqry.query("DELETE FROM users.groups WHERE key = ?", group.key);
        mqry.execute();

    }

    @Override
    protected JObject doLoadUserConfig(BaseUserData user) throws Exception {
        return null;
    }

    @Override
    protected void doSaveUserConfig(BaseUserData user, JObject data) throws Exception {
    }

    @Override
    public void doExportUsers(HttpRequest request) throws Exception {

    }

    private final static String USERS_QUERY = "SELECT * , \n"
            + "        (SELECT group_concat(right_key || ':' || refused)\n"
            + "        FROM users.user_rights ur\n"
            + "        WHERE ur.user_id = u.user_id) as rights,\n"
            + "        (SELECT group_concat(group_key)\n"
            + "        FROM users.user_groups gr\n"
            + "        WHERE gr.user_id = u.user_id) as groups      \n"
            + "FROM users.users u\n"
            + "WHERE ?\n"
            + "ORDER BY user_id";

}

package com.user;

import com.servlet.controller.Page;
import com.servlet.controller.BaseSession;
import com.utils.collections.Pair;
import com.utils.hashes.Hashes;
import com.utils.hashes.Hex;
import com.html.modules.WindowLayer;
import com.resources.Res;
import com.exceptions.http.Http400BadRequestException;
import com.html.core.styles.FontWeight;
import com.html.core.tag.form.DataList;
import com.html.core.tag.form.Form;
import com.html.core.tag.form.input.InputText;
import com.html.core.tag.intfs.Container;
import com.html.core.tag.semantic.CTag;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.html.js.Ajax;
import com.html.js.Call;
import com.lang.LUser;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.user.right.RUsers;
import com.user.right.UserRight;
import com.utils.collections.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.Map.Entry;

import static com.resources.Res.*;

@Endpoint(url = {"$users/edit", "$users/new"},
        title = "Edycja użytkownika", rights = RUsers.class)
@Deprecated
public class PUserEdit extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        BaseUsersHandler handler = BaseUsersHandler.instance();

        String userName = params.firstStr("");

        BaseUserData usr = null;
        if (!userName.isEmpty()) {
            usr = handler.getUser(userName);
            if (usr == null)
                throw new Http400BadRequestException(LUser.USER_NOT_FOUND_ARG.toString(userName));

        }

        WindowLayer layer = new WindowLayer(this);
        layer.caption = usr == null
                ? LUser.NEW_USER.toString()
                : LUser.EDIT_USER.toString() + usr.username;

        link(utils, Res.layer, jQuery);

        link("/res/skin.css",
                "/res/service/users/users.css",
                "/res/service/users/users.js");

        boolean isRoot = usr != null && usr.username.equalsIgnoreCase("ROOT");

        Form frm = layer.form;
        frm.action(getUrl("$users/edit?setUser=" + (usr != null ? usr.id : "")));
        Div dButtons = frm.div();

        dButtons
                .span()
                .text(LUser.DATA.toString())
                .id("btnPageData")
                .cls("btnPage")
                .onClick(new Call("setActivePage", "data")).attrs.set("active", "");

        dButtons
                .span()
                .text(LUser.PERMISSIONS.toString())
                .id("btnPageRights")
                .onClick(new Call("setActivePage", "rights"))
                .cls("btnPage");

        Div tUser = frm.div().id("pnlUserData");
        buildUserRightsPanel(usr, frm.div().id("pnlUserRights"));

        Div div = tUser.div().cls("edit_group");

        InputText edt = div.inputText()
                .readOnly(isRoot)
                .value(usr != null ? usr.username : null)
                .size(40)
                .required(true)
                .name("login");
        edt.labelBefore(LUser.LOGIN.toString() + ":");

        DataList dl = div.dataList().addAll(BaseLdap.newInstance().getUsers());

        edt.list(dl);

        div = tUser.div().cls("edit_group");
        div.span(LUser.DISPLAY_NAME.toString() + ":");
        div.inputText()
                .size(40)
                .name("displayName")
                .required(true)
                .value(usr != null ? usr.displayName : null);

        if (handler.features.passwordChange) {
            div = tUser.div().cls("edit_group");
            div.span(LUser.PASSWORD.toString() + ":");
            div.inputText()
                    .size(40)
                    .name("password")
                    .value(usr != null ? usr.passHash : null);
            handler.editUserPage(usr, frm);
        }

        div = tUser.div().cls("edit_group");
        div.span(LUser.FIRST_NAME.toString() + ":");
        div.inputText()
                .readOnly(isRoot)
                .value(usr != null ? usr.firstname : null)
                .size(40)
                .required(true)
                .name("firstname");

        div = tUser.div().cls("edit_group");
        div.span(LUser.SURNAME.toString() + ":");
        div.inputText()
                .readOnly(isRoot)
                .value(usr != null ? usr.lastname : null)
                .size(40)
                .required(true)
                .name("lastname");

        div = tUser.div().cls("edit_group");
        div.span().text(LUser.EMAIL.toString() + ":");
        div.inputText()
                .readOnly(isRoot)
                .value(usr != null ? usr.email : null)
                .size(40)
                .required(true)
                .name("email");

        if (!isRoot) {

            tUser.inputCheckBox()
                    .checked(usr != null ? usr.status == UserStatus.active : true)
                    .name("enabled")
                    .labelAfter(LUser.ACTIVE.toString());

            tUser.br();
            tUser.br();
        }

        MapList<String, Pair<Field, UserDataField>> fields
                = new MapList<>(new TreeMap<String, TList<Pair<Field, UserDataField>>>());

        for (Field f : handler.getClass().getFields()) {
            UserDataField ann = f.getAnnotation(UserDataField.class);
            if (ann != null)
                fields.add(ann.group(), new Pair<>(f, ann));
        }

        for (Entry<String, TList<Pair<Field, UserDataField>>> en : fields) {

            Container tgroup = en.getKey().isEmpty() ? tUser
                    : tUser.fieldset(en.getKey());

            for (Pair<Field, UserDataField> p : en.getValue()) {

                div = tgroup.div().cls("edit_group");
                div.span(p.second.title());
                div.inputText()
                        .size(40)
                        .name(p.second.key().isEmpty() ? p.first.getName()
                                : p.second.key())
                        .value(usr != null ? p.first.get(usr) : null);

            }

            tgroup.br();

        }

        Map<String, String> map = new LinkedHashMap<>();
        //   for (String s : handler.getUserAttributes())
        //      map.put(s, "");

        if (usr != null)
            map.putAll(usr.attributes);

        if (!map.isEmpty()) {
            tUser.div().text(LUser.ATTRIBUTES.toString() + ":").style().fontWeight(FontWeight.bold).marginBottom("4px");

            Table tbl = tUser.table();
            for (Map.Entry<String, String> en : map.entrySet()) {
                Tr tr = tbl.tbodyTr();
                tr.td().text(en.getKey() + ": ");
                tr.td().inputText()
                        .size(40)
                        .value(en.getValue()).name("attr_" + en.getKey());
            }
            tUser.br();
        }

        layer.addSubmitButton(null, usr == null ? LUser.ADD.toString() : LUser.ACCEPT.toString());
        layer.addCloseButton(null, LUser.CLOSE.toString());

        if (usr != null && !isRoot && handler.features.removeUser)
            layer.addButton(Res.remove16png, null, LUser.REMOVE.toString(), false,
                    new Ajax("./$users/edit")
                            .param("removeUser", usr.username)
                            .confirm(LUser.CONFIRM_USER_DELETE.toString(
                                    usr.username + "\"?")));

    }

    private void buildUserRightsPanel(final BaseUserData user, CTag tag) {

        body.br();
        tag.br();
        tag.span(LUser.GROUPS2.toString() + ":");

        tag.hr();

        for (RightsScheme group : RightsScheme.getAll())
            tag.inputCheckBox()
                    .checked(user != null && user.rights.groups.contains(group))
                    .name("group")
                    .value(group.key)
                    .labelAfter(group.name);

        tag.br();
        tag.br();
        tag.span(LUser.ROLES.toString() + ":");
        tag.hr();

    }

    @Endpoint
    public void setUser() throws Exception {

        BaseUsersHandler handler = BaseUsersHandler.instance();

        boolean isNew = true;

        String login = params.getStr("login").trim().toLowerCase();
        if (login.isEmpty())
            throw new Error(LUser.LOGIN_CANT_BE_EMPTY.toString());

        BaseUserData usr = null;
        if (isNew)
            usr = handler.newUserData();
        else {
            usr = handler.getUser(login);
            if (usr == null)
                throw new Error(LUser.USER_NOT_FOUND_ARG.toString(login));
        }

        if (usr.username != null && usr.username.equalsIgnoreCase("ROOT"))
            user.rights.check(UserRight.root);

        usr.attributes.clear();
        /* for (String s : getHandler().getUserAttributes()) {
         String v = params.getStr("attr_" + s, null);
         if (v != null)
         usr.attributes.put(s, v);
         }
         */
        usr.username = login;
        usr.status = params.getBool("enabled", false) ? UserStatus.active : UserStatus.disabled;

        if (usr.username != null && usr.username.equalsIgnoreCase("ROOT")) {
            usr.rights.allow(UserRight.root);
            usr.status = UserStatus.active;
        } else {
            usr.rights.groups.clear();
            usr.rights.groups.addAll(RightsScheme.asList(params.getList("group"), usr.username));
            /*
            for (RightsScheme group : usr.groups)
                for (Role role : Role.getAll(group.roles))
                    if (!session.user.hasRights(role))
                        throw new Http403ForbiddenException("Nie posiadasz uprawnień "
                                + "do nadania grupy \"" + group.name + "\"");

            if (session.user.hasRights(Role.roles)) {

                usr.rights.clear();
                usr.refusedRoles.clear();

                for (Role r : Role.root.getAll()) {

                    Boolean val = params.getBool("r_" + r.key, null);

                    if (val == null)
                        continue;

                    if (!session.user.hasRights(Role.roles))
                        throw new Http403ForbiddenException("Nie posiadasz uprawnień "
                                + "do nadania ról");

                    if (val)
                        usr.rights.add(r);

                    if (!val)
                        usr.refusedRoles.add(r);
                }
            }
             */

        }

        usr.firstname = params.getStr("firstname", "");
        usr.lastname = params.getStr("lastname", "");
        usr.email = params.getStr("email", "");

        usr.displayName = params.getStr("displayName", "");
        if (usr.displayName.isEmpty())
            usr.displayName = usr.username;

        if (handler.features.passwordChange) {
            String pass = params.getStr("password", "");
            boolean useHash = (isNew || pass.length() != 32) && !pass.isEmpty();
            if (!isNew && !useHash)
                try {
                    Hex.toBytes(pass);
                } catch (Exception e) {
                    useHash = true;
                }

            usr.passHash = useHash ? Hashes.hash(Hashes.Hash.MD5, pass) : pass;
        }

        handler.editUser(usr, isNew);

        for (BaseSession ss : BaseSession.getSessions())
            if (ss.user != null && usr.username.equals(ss.user.username)) {
                ss.user.authorized = false;
                return;
            }

    }

}

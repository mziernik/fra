package com.user.pages;

import com.user.RightsScheme;
import com.utils.Utils;
import com.utils.Is;
import com.config.CAdministration;
import com.config.CAdministration.CRegistration;
import com.config.CAdministration.CSMTP;
import com.config.CHttp;
import com.config.CService;
import com.context.AppConfig;
import com.context.AppContext;
import com.exceptions.http.Http400BadRequestException;
import com.extensions.email.Email;
import com.extensions.email.EmailType;
import com.html.core.Html;
import com.html.core.dict.InputType;
import com.html.core.styles.TextAlign;
import com.html.core.styles.VerticalAlign;
import com.html.core.tag.form.Form;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.semantic.Span;
import com.html.core.tag.table.*;
import com.html.js.*;
import com.html.modules.CenterDialog;
import com.html.modules.LayerDialog;
import com.html.modules.LayerDialog.MessageType;
import com.json.JObject;
import com.resources.Res;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.servlet.interfaces.HttpMethod;
import com.servlet.requests.HttpRequest;
import com.user.*;
import com.user.right.RUsers;
import com.utils.Url;
import com.utils.collections.Pair;
import com.utils.date.TDate;
import com.utils.hashes.Hashes;
import java.io.File;
import java.util.LinkedList;

@Endpoint(url = "$signup", auth = false, resources = "service", title = "Rejestracja")
public class PSignup extends Page {

    private final static Object sync = new Object();
    private File file = AppContext.varPath.getFile("registrations.json");

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        if (!CRegistration.enabled.value(false))
            throw new Http400BadRequestException(request);

        head.script("function loginChange() {\n"
                + "    var login = $id(\"login\");\n"
                + "    if (!login)\n"
                + "        return;\n"
                + "\n"
                + "    login = login.value;\n"
                + "    if (!login.trim())\n"
                + "        return;\n"
                + "\n"
                + "    ajax.post(\"?verifyUser\", {\n"
                + "        login : login\n"
                + "    }, function(http) {\n"
                + "        if (http.error)\n"
                + "            return;\n"
                + "\n"
                + "        var data = JSON.parse(http.responseText);\n"
                + "\n"
                + "        for (var name in data) {\n"
                + "            var edt = $id(name);\n"
                + "            if (!edt)\n"
                + "                continue;\n"
                + "            edt.value = data[name];\n"
                + "        }\n"
                + "    });\n"
                + "}");

        CenterDialog window = new CenterDialog(this, null, "btnRegister");

        Table tbl = window.table;

        window.header.text("Formularz rejestracyjny").colspan(2);

        Form form = window.form;
        form.action(new Url("?submit"));

        String username = params.getStr("username", "");

        Load onClose = new Load(CHttp.url.value());
        if (!username.isEmpty())
            onClose.param("username", username);

        form.onSubmit(
                new Ajax("?submit")
                        .onDone(
                                new Layer("?done")
                                        .onClose(onClose)
                        ),
                new Eval("return false"));

        tbl.tbodyTr().td().colspan(2);

        Tr tr = tbl.tbodyTr();
        tr.td("Login:");
        tr.td().inputText()
                .id("login")
                .name("login")
                .value(username)
                .required(true)
                .onChange(new Call("loginChange"))
                .dataList()
                .addAll(BaseLdap.newInstance().getUsers());

        if (CRegistration.passwordRequired()) {
            tr = tbl.tbodyTr();
            tr.td("Hasło:");
            tr.td().inputPassword()
                    .id("password1")
                    .name("password")
                    .required(true);

            tr = tbl.tbodyTr();
            tr.td("Powtórz hasło:");
            tr.td().inputPassword()
                    .id("password2")
                    .name("password")
                    .required(true);

        }

        tr = tbl.tbodyTr();
        tr.td("Imię:");
        tr.td().inputText()
                .id("firstname")
                .name("firstname")
                .spellcheck(true)
                .required(true);

        tr = tbl.tbodyTr();
        tr.td("Nazwisko:");
        tr.td().inputText()
                .id("lastname")
                .name("lastname")
                .spellcheck(true)
                .required(true);

        tr = tbl.tbodyTr();
        tr.td("E-mail:");
        tr.td().inputText()
                .type(InputType.email)
                .id("email")
                .name("email")
                .required(true);

        LinkedList<Pair<Boolean, String>> groups = new LinkedList<>(CRegistration.groups.getValue(null));
        if (!groups.isEmpty()) {

            tr = tbl.tbodyTr();

            tr.td("Grupy:");

            Td td = tr.td();

            for (Pair<Boolean, String> p : groups) {
                if (p.first == null || !p.first)
                    continue;

                RightsScheme group = RightsScheme.get(p.second, true);
                if (group == null)
                    continue;

                Span lbl = td.div().span();
                lbl.title(group.details)
                        .inputCheckBox()
                        .name("group")
                        .value(group.key);
                lbl.span(group.name);
            }
        }

        tr = tbl.tbodyTr();

        tbl.tbodyTr().td().colspan(2);

        tr.td().colspan(2)
                .inputSubmit()
                .id("btnRegister")
                .value("Zarejestruj");

        if (!username.isEmpty())
            body.script("loginChange();");
    }

    @Endpoint(auth = false)
    public void verifyUser() throws Exception {
        String login = params.getStr("login").trim();

        BaseUserData user = BaseUserData.newInstance();
        user.username = login;
        BaseLdap.newInstance().getUserInfo(user);

        JObject json = new JObject();

        if (user.firstname != null && !user.firstname.trim().isEmpty())
            json.put("firstname", user.firstname.trim());

        if (user.lastname != null && !user.lastname.trim().isEmpty())
            json.put("lastname", user.lastname.trim());

        if (user.email != null && !user.email.trim().isEmpty())
            json.put("email", user.email.trim().toLowerCase());

        returnJson(json);
    }

    @Endpoint(methods = HttpMethod.POST, auth = false)
    public void submit() throws Exception {

        if (!CRegistration.enabled.value(false))
            throw new Http400BadRequestException(request);

        String login = params.getStr("login").trim().toLowerCase();
        LinkedList<String> pass = params.getList("password");
        String email = params.getStr("email").trim().toLowerCase();
        String firstname = params.getStr("firstname").trim();
        String lastname = params.getStr("lastname").trim();

        BaseUsersHandler handler = BaseUsersHandler.instance();
        handler.checkName(login);

        if (CRegistration.passwordRequired()
                && (pass.size() != 2 || !pass.get(0).equals(pass.get(1))))
            throw new Error("Hasła nie są identyczne");
        JObject json;

        synchronized (sync) {
            json = JObject.load(file, true);
        }

        for (JObject obj : json.getObjects())
            if (obj.getStr("login").equals(login))
                throw new Error("Wniosek o rejestrację użytkownika \""
                        + login + "\" został już zgłoszony");

        BaseLdap.newInstance().checkUserName(login);

        String id = Utils.randomId(20);

        JObject obj = json.objectC(id);
        obj.put("login", login);
        if (!pass.isEmpty())
            obj.put("password", Hashes.md5(pass.peek()));
        obj.put("firstname", firstname);
        obj.put("lastname", lastname);
        obj.put("email", email);
        obj.put("date", new TDate().toString(true));
        obj.put("ip", request.request.getRemoteAddr());
        obj.put("ua", request.userAgent.toString());
        obj.put("groups", params.getList("group"));

        if (!CRegistration.emailVerification.value(false)) {
            confirm(json, id);

            synchronized (sync) {
                json.write(file, false);
            }

            return;
        }

        synchronized (sync) {
            json.write(file, false);
        }

        Email mail = new Email();
        mail.addRecipient(EmailType.TO, email);
        mail.setTitile("Rejestracja w serwisie " + AppConfig.getServiceTitle());

        Html html = new Html();

        html.body.span("Kliknij w ten");
        html.body.a("link").href(CHttp.url(http(), "$signup?confirm=" + id, true));
        html.body.span("aby aktywować konto w serwisie " + AppConfig.getServiceTitle());
        mail.send(html);
    }

    @Endpoint(auth = false)
    public void done() throws Exception {

        if (!CRegistration.enabled.value(false))
            throw new Http400BadRequestException(request);

        LayerDialog layer = new LayerDialog(this, MessageType.info);
        layer.caption = "Rejestracja";

        if (CRegistration.emailVerification.value())
            body.h4("Na podany adres e-mail została wysłana wiadomość "
                    + "z linkiem weryfikacyjnym");
        else {

            body.center().h3("Konto zostało utworzone.");
            body.div("Wniosek wymaga jeszcze "
                    + "akceptacji administratora usługi.");
            body.div("Po zaakceptowaniu lub "
                    + "odrzuceniu wniosku otrzymasz wiadomość e-mail informującą o statusie.");
        }
    }

    @Endpoint(auth = false)
    public void confirm() throws Exception {

        if (!CRegistration.enabled.value(false))
            throw new Http400BadRequestException(request);

        String id = params.getStr("confirm");

        JObject json;

        synchronized (sync) {
            json = JObject.load(file, true);
        }
        confirm(json, id);
    }

    /**
     * Email dla adminów informujący o nowym koncie
     *
     * @param json
     * @param id
     * @throws Exception
     */
    private void confirm(JObject json, String id) throws Exception {

        if (!CRegistration.enabled.value(false))
            throw new Http400BadRequestException(request);

        JObject data = json.object(id);

        if (data == null)
            throw new Http400BadRequestException(request);

        Email mail = new Email(CAdministration.emailList);

        mail.setFrom(CSMTP.getSender());

        String title = "Rejestracja użytkownika \"" + data.getStr("login")
                + "\" w serwisie " + AppConfig.getServiceTitle();
        mail.setTitile(title);

        Html html = new Html();

        html.body.h4(title);

        Table tbl = html.body.table();

        html.head.styles("body", "table")
                .fontSize("10pt")
                .fontFamilySans()
                .lineHeight("1.5em");

        tbl.tbodyTr().setCells("Login:", data.getStr("login"));
        tbl.tbodyTr().setCells("Imię:", data.getStr("firstname"));
        tbl.tbodyTr().setCells("Nazwisko:", data.getStr("lastname"));
        tbl.tbodyTr().setCells("E-mail:", data.getStr("email"));
        tbl.tbodyTr().setCells("Data:", data.getStr("date"));
        tbl.tbodyTr().setCells("IP:", data.getStr("ip"));
        tbl.tbodyTr().setCells("UA:", data.getStr("ua"));

        if (data.has("groups")) {
            Tr tr = tbl.tbodyTr();
            tr.td("Grupy:").style().verticalAlign(VerticalAlign.top);
            Td td = tr.td();

            for (String s : data.arrayD("groups").getValuesStr()) {
                RightsScheme group = RightsScheme.get(s, true);
                if (group != null)
                    td.div(group.name);
            }

        }

        html.body.hr();

        html.body.span("Kliknij w ten");
        html.body.a("link").href(CHttp.url(http(), "$signup?manage=" + id, true));
        html.body.span("aby zatwierdzić lub odrzucić wniosek.");
        mail.send(html);

        Div center = body.div();
        center.style().textAlign(TextAlign.center);

        center.h2("Konto zostało potwierdzone.");
        center.h3("Wniosek wymaga jeszcze "
                + "akceptacji administratora usługi.");
        center.h3("Po zaakceptowaniu lub "
                + "odrzuceniu wniosku otrzymasz wiadomość e-mail informującą o statusie.");

    }

    @Endpoint(auth = true, rights = RUsers.class)
    public void manage() throws Exception {

        String sel = params.getStr("manage", null);

        JObject json;

        synchronized (sync) {
            json = JObject.load(file, true);
        }

        link(Res.utils);

        Table tbl = body.table();
        tbl.cls("tbl tblSel");
        tbl.style().width("100%");

        tbl.theadTr().setCells("Data", "ID", "IP", "Login", "Email", "Imię i nazwisko", "");

        for (JObject obj : json.getObjects()) {
            String id = obj.getName();

            Tr tr = tbl.theadTr();

            tr.td(obj.getDate("date").toString(false));
            tr.td(id);
            tr.td(obj.getStr("ip"));
            tr.td().b(obj.getStr("login"));
            tr.td(obj.getStr("email"));
            tr.td(obj.getStr("firstname") + " " + obj.getStr("lastname"));
            Td td = tr.td();
            td.button("Zatwierdź").onClick(new Ajax("?accept")
                    .param("id", id).confirm("Czy na pewno zatwierdzić wniosek użytkownika \""
                    + obj.getStr("login") + "\"?")
            //  .onDone(new Load(PUsers.class).param("username", obj.getStr("login"))
            //  )
            );

            td.button("Odrzuć").onClick(new Ajax("?reject")
                    .param("id", id).confirm("Czy na pewno odrzucić wniosek użytkownika \""
                    + obj.getStr("login") + "\"?")
                    .reload(true)
            );

            if (id.equals(sel))
                tr.cls("green");
        }

        //  body.button("Użytkownicy").onClick(new Load(PUsers.class).newWindow(true));
    }

    @Endpoint(auth = true, rights = RUsers.class)
    public void reject() throws Exception {
        String id = params.getStr("id");
        synchronized (sync) {
            JObject json = JObject.load(file, true);

            JObject obj = json.object(id);
            if (obj == null)
                throw new Http400BadRequestException(request);

            json.remove(id);
            json.write(file, false);

            Email mail = new Email();

            mail.addRecipient(EmailType.TO, obj.getStr("email"));

            mail.setFrom(CSMTP.getSender());
            mail.setTitile("Rejestracja w serwisie " + AppConfig.getServiceTitle() + " odrzucona");

            Html html = new Html();

            html.body.h4("Wniosek o rejestrację użytkownika \"" + obj.getStr("username") + "\" w serwisie "
                    + AppConfig.getServiceTitle() + " został odrzucony");
            mail.send(html);
        }
    }

    /**
     * Akceptacja, założenie konta użytkownika
     *
     * @throws Exception
     */
    @Endpoint(auth = true, rights = RUsers.class)
    public void accept() throws Exception {
        String id = params.getStr("id");
        synchronized (sync) {
            JObject json = JObject.load(file, true);

            JObject obj = json.object(id);
            if (obj == null)
                throw new Http400BadRequestException(request);

            BaseUsersHandler handler = BaseUsersHandler.instance();
            BaseUserData usr = handler.newUserData();

            usr.status = UserStatus.active;
            usr.created = obj.getDate("date").defaultFormat(TDate.FULL);
            usr.createdBy = user.username;
            usr.passPlain = obj.getStr("password", usr.passPlain);
            usr.username = obj.getStr("login");
            usr.email = obj.getStr("email");
            usr.firstname = obj.getStr("firstname");
            usr.lastname = obj.getStr("lastname");
            usr.displayName = usr.firstname + " " + usr.lastname;

            for (String s : obj.arrayD("groups").getValuesStr())
                usr.rights.addGroup(RightsScheme.get(s, true));

            handler.editUser(usr, true);

            json.remove(id);
            json.write(file, false);

            Email mail = new Email();

            mail.addRecipient(EmailType.TO, usr.email);

            mail.setFrom(CSMTP.getSender());
            mail.setTitile("Rejestracja w serwisie " + AppConfig.getServiceTitle() + " zaakceptowana");

            Html html = new Html();

            html.body.h4("Wniosek o rejestrację użytkownika \"" + usr.username + "\" w serwisie "
                    + AppConfig.getServiceTitle() + " został zaakceptowany.");
            html.body.span("Kliknij ");
            html.body.a("tutaj").href(CHttp.url(http(), "", null).param("username", usr.username));
            html.body.span("aby się zalogować.");
            mail.send(html);

        }
    }

}

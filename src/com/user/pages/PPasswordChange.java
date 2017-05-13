package com.user.pages;

import com.utils.Utils;
import com.utils.Is;
import com.config.CAuthorization;
import com.context.AppConfig;
import com.servlet.controller.Page;
import com.exceptions.http.Http404FileNotFoundException;
import com.exceptions.http.Http401UnauthorizedException;
import com.html.core.dict.*;
import com.html.core.styles.*;
import com.html.core.tag.form.Fieldset;
import com.html.core.tag.form.Form;
import com.html.core.tag.semantic.Div;
import com.lang.LUser;
import com.servlet.interfaces.*;
import com.user.BaseUserData;
import com.user.BaseUsersHandler;
import com.mlogger.Log;
import com.net.HTTP;
import com.resources.Res;
import com.servlet.Handlers;
import com.servlet.requests.HttpRequest;
import com.user.UserStatus;
import com.user.right.UserRight;
import com.utils.Url;

@Endpoint(url = "$passchange", auth = false, ssl = true, title = "Zminana hasła")
public class PPasswordChange extends Page {

    Exception ex;
    protected Div flexTop;
    protected Div flexBottom;

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        head.link(Res.bootstrap);
        BaseUserData usr = user;

        String username = params.getStr("user", "");
        if (!username.isEmpty())
            usr = BaseUsersHandler.instance().getUser(username);

        if (usr == null || !usr.exists)
            usr = BaseUsersHandler.instance().getUser(params.getStr("user", null));

        if ((usr == null || !usr.exists) && "root".equals(username)) {
            if (usr == null)
                usr = new BaseUserData(null);
            usr.username = "root";
            usr.rights.allow(UserRight.root);
            usr.displayName = "ROOT";
            usr.firstname = "Root";
            usr.lastname = "Root";
            usr.status = UserStatus.active;
        }

        if (usr == null)
            throw new Http404FileNotFoundException(LUser.USER_NOT_FOUND_ARG.toString(""));

        boolean newPass = usr.passHash == null || usr.passHash.isEmpty();

        head.title(AppConfig.getServiceTitle() + " :: " + (newPass
                ? LUser.CREATE_USER_PASSWORD.toString(usr.displayName)
                : LUser.CHANGE_USER_PASSWORD.toString(usr.displayName)));

        if (params.getStr("apply", "").equals("true")) {
            apply(usr);
            if (ex == null)
                return;
        }

        body.style()
                .position(Position.absolute)
                .width("100%")
                .height("100%")
                .margin("0")
                .padding("0")
                .display(Display.flex)
                .flexDirection(FlexDirection.column);

        flexTop = body.div();
        flexTop.style()
                .flex(Flex.auto)
                .flexGrow(1);
        Div container = body.div()
                .cls("container");
        flexBottom = body.div();
        flexBottom.style()
                .flex(Flex.auto)
                .flexGrow(2);

        if (ex != null) {
            Div alert = container.div().cls("alert alert-danger fade in");
            alert.style().textAlign(TextAlign.center);
            alert.a().hrefVoid()
                    .cls("close")
                    .data("dismiss", "alert")
                    .innerHTML("&times;");

            alert.strong(Handlers.errors.getInstance().getMessage(ex).toString());
        }

        Div panel = container
                .div().cls("col-xs-12 col-sm-offset-3 col-sm-6")
                .div().cls("panel panel-default");

        panel.div().cls("panel-heading")
                .strong(AppConfig.getServiceTitle() + " :: " + (newPass
                        ? LUser.CREATE_USER_PASSWORD.toString(usr.displayName)
                        : LUser.CHANGE_USER_PASSWORD.toString(usr.displayName)));

        Form form = panel.div().cls("panel-body").form();

        form.method(MethodType.post);
        form.inputHidden().name("authRequest").value(Utils.randomId());
        boolean ssl = request.isSecure() || CAuthorization.ssl.value();
        form.method(MethodType.post)
                .action(getAbsoluteURL(request.url, ssl));

        String redirect = params.getStr("redirect", "");
        form.action(PPasswordChange.class)
                .param("apply", true)
                .param("user", usr.username)
                .param(redirect.isEmpty() ? null : "redirect", redirect);

        ///-------------------------------------------------------------
        Fieldset fSet = form.fieldset(null);

        Div div = fSet.div().cls("row")
                .div().cls("col-sm-12 col-md-10 col-md-offset-1");

        Div group = div.div().cls("form-group")
                .div().cls("input-group");

        group.span().cls("input-group-addon").i().cls("glyphicon glyphicon-lock");
        group.inputPassword().cls("form-control")
                .placeholder(LUser.PASSWORD.toString())
                .name("password1")
                .required(true);

        group = div.div().cls("form-group")
                .div().cls("input-group");

        group.span().cls("input-group-addon").i().cls("glyphicon glyphicon-lock");
        group.inputPassword().cls("form-control")
                .placeholder(LUser.PASSWORD_REPEAT.toString())
                .name("password2")
                .required(true);

        group = div.div().cls("form-group");

        group.inputSubmit()
                .cls("btn btn-lg btn-primary")
                .value(LUser.APPLY.toString())
                .style().float_(SFloat.right);

        //--------------
        Div footer = panel.div().cls("panel-footer");

        returnHTML(HTTP.unauthorized401.code);
    }

    protected void apply(BaseUserData usr) throws Exception {
        BaseUsersHandler users = BaseUsersHandler.instance();
        ex = null;
        try {
            boolean newPass = usr.passHash == null || usr.passHash.isEmpty();

            if (!newPass)
                if (!users.getHash(params.getStr("password0")).equals(usr.passHash))
                    throw new Http401UnauthorizedException(LUser.INCORRECT_PASSWORD.toString());

            String p1 = params.getStr("password1");
            String p2 = params.getStr("password2");
            users.verifyPassword(p1);

            if (!p1.equals(p2))
                throw new Http401UnauthorizedException(LUser.PASSWORDS_MUST_BE_EQUAL.toString());

            usr.passHash = users.getHash(p1);
            users.editUser(usr, !usr.exists);

            String redirect = params.getStr("redirect", "");
            if (redirect != null)
                redirect(new Url(redirect));

        } catch (Exception e) {
            Log.warning(e);
            ex = e;
        }
    }

}

package com.user.pages;

import com.servlet.controller.BaseSession;
import com.exceptions.EError;
import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.config.*;
import com.context.AppConfig;
import com.exceptions.http.Http401UnauthorizedException;
import com.html.core.dict.*;
import com.html.core.styles.*;
import com.html.core.tag.form.Fieldset;
import com.html.core.tag.form.Form;
import com.html.core.tag.semantic.Div;
import static com.lang.LController.*;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.net.HTTP;
import com.resources.Res;
import com.servlet.Handlers;
import com.servlet.controller.*;
import com.servlet.requests.HttpRequest;
import com.utils.Url;
import java.io.IOException;

@Endpoint(url = {"$login"}, ssl = true, auth = false)
public class PLogin extends Page {

    protected Div flexTop;
    protected Div flexBottom;

    public static void setLoginError(BaseSession session, Throwable e) {
        if (e == null)
            session.data.remove("$loginException");
        else
            session.data.put("$loginException", e);
    }

    public static void redirect(Controller current) throws IOException {

        HttpRequest http = current.http();

        String redirectUrl = http.params.getStr("redirect", "");

        if (Is.empty(redirectUrl) && !(current instanceof PLogin))
            redirectUrl = http.url.toString();

        String mainUrl = CHttp.url.value().toString();

        if (mainUrl != null && mainUrl.contains("://") && redirectUrl.contains("://")) {

            Url mUrl = new Url(mainUrl);
            Url rUrl = new Url(redirectUrl);

            mUrl.port(rUrl.port());
            mUrl.protocol(rUrl.protocol());

            if (mUrl.equals(rUrl))
                redirectUrl = null;
        }

        Url url = new Url(PLogin.class);
        if (!Is.empty(redirectUrl))
            url.param("redirect", redirectUrl);

        current.http().redirect(url);
    }

    /*
     @Endpoint(url = "auth", title = "Autoryzacja", auth = false, methods = HttpMethod.POST)
     public void auth() throws Exception {

     }
     */
    @Override
    public void onRequest(HttpRequest http) throws Exception {
        head.link(Res.bootstrap);
        if (params.has("authRequest")) {
            setLoginError(session, null);
            try {
                if (!Handlers.auth.getInstance().authorize(this, user, true))
                    onRequest(http);

                String ret = params.getStr("redirect", "");

                redirect(ret.isEmpty() ? CHttp.url.value() : new Url(ret));

            } catch (Exception e) {
                if (!http.isLocked())
                    onRequest(e);
            }
            return;
        }

        onRequest((Throwable) null);
    }

    private void onRequest(Throwable ex) throws Exception {

        if (ex == null)
            ex = (Exception) session.data.get("$loginException");

        head.link(Res.bootstrap);
        head.title(LOGIN__SIGN_IN);
        head.meta("viewport", "width=device-width, initial-scale=1.0");

        if (user.username == null || user.username.isEmpty()) {

            //ToDo pobieranie danych użytkownika z sessji
        }

        if (CAuthorization.htAccess.value(false)) {
            setHeader("WWW-Authenticate", "Basic realm=\"Usluga "
                    + StrUtils.convertPolishChars(AppConfig.getServiceTitle())
                    + " wymaga autoryzacji\"");

            throw new Http401UnauthorizedException();
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
                .flexGrow(1);

        if (ex != null) {
            Div alert = container.div().cls("alert alert-danger fade in");
            alert.style().textAlign(TextAlign.center);
            alert.a().hrefVoid().cls("close").data("dismiss", "alert").innerHTML("&times;");
            alert.strong(EError.toString(ex));
        }

        Div panel = container
                .div().cls("col-xs-12 col-sm-offset-3 col-sm-6")
                .div().cls("panel panel-default");

        panel.div().cls("panel-heading").strong(AppConfig.getServiceTitle() + " :: " + LOGIN__AUTHORIZATION);

        Form form = panel.div().cls("panel-body").form();

        //   form.div("Wymagana autoryzacja");
        form.method(MethodType.post);
        form.inputHidden().name("authRequest").value(Utils.randomId());
        boolean ssl = request.isSecure() || CAuthorization.ssl.value();
        form.method(MethodType.post)
                .action(getAbsoluteURL(request.url, ssl));

        String redirect = params.getStr("redirect", "");
        form.action(PLogin.class)
                .param(redirect.isEmpty() ? null : "redirect", redirect);

        ///-------------------------------------------------------------
        Fieldset fSet = form.fieldset(null);

        //  fSet.div().cls("row").div("center-block");
        Div div = fSet.div().cls("row")
                .div().cls("col-sm-12 col-md-10  col-md-offset-1");

        Div group = div.div().cls("form-group")
                .div().cls("input-group");

        String username = params.getStr("username", user.username);

        group.span().cls("input-group-addon").i().cls("glyphicon glyphicon-user");
        group.inputText().cls("form-control")
                .placeholder(LOGIN__USERNAME.toString())
                .name("username")
                .value(username)
                .autofocus(Is.empty(username))
                .required(true);

        group = div.div().cls("form-group")
                .div().cls("input-group");
        group.span().cls("input-group-addon").i().cls("glyphicon glyphicon-lock");
        group.inputPassword().cls("form-control")
                .placeholder(LOGIN__PASSWORD.toString())
                //  .value(user.passPlain)
                .autofocus(!Is.empty(username))
                .name("password")
                .required(true);

        group = div.div().cls("form-group");

        group.inputSubmit()
                .cls("btn btn-lg btn-primary")
                .value(LOGIN__DO_LOGIN)
                .style().float_(SFloat.right);

        //--------------
        Div footer = panel.div().cls("panel-footer");
        if (CAdministration.CRegistration.enabled.value(false))
            footer.a().cls("btn btn-link").text("Rejestracja").href(PSignup.class);
        //     footer.a().cls("btn btn-link").text("Kontakt z administratorem");

        returnHTML(HTTP.unauthorized401.code);

    }
}

package com.user;

import com.servlet.controller.Page;
import com.html.core.styles.Position;
import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.Div;
import com.html.js.Ajax;
import com.lang.LUser;

import static com.resources.Res.*;

import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.user.right.UserRight;

/**
 * Miłosz Ziernik 2013/03/28
 */
@Endpoint(url = "$user", title = "Użytkownik")
public class PLoggedUser extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        link(utils, layer);

        body.style().fontSize("10pt").fontFamilySans();
        body.h3().text(session.user.displayName + " (" + session.user.username + ")");
        Div dButtons = body.div();
        dButtons.style().position(Position.absolute).top("8px").right("8px");

        //   BServiceConfig.CManager cmgr = BaseContext.config.service.manager;
        /*
         if (session.user.hasRights(Role.manager)
         && cmgr.reloadScript.value() != null
         && !cmgr.reloadScript.value().trim().isEmpty())
         dButtons.button().text("Restart serwera")
         .onClick(JsUtils.confirmAndReload("Czy na pewno zrestartować serwer?",
         new JsUtils.UrlBuilder("$manager?restart"),
         null, null)).cls("btn");
         */
        dButtons.inputButton()
                .value(LUser.NEW_SESSION.toString())
                .onClick(new Ajax("/$?invalidateSession"));

        dButtons.inputButton()
                .value(LUser.LOGOUT.toString())
                .value("service.logout()");

        body.br();
        body.br();
        body.div().text(LUser.SESSION.toString());

        if (!session.user.rights.groups.isEmpty()) {
            body.hr();
            body.h3().text(LUser.GROUPS2.toString() + ":");
            Ul ul = body.ul();
            for (RightsScheme group : session.user.rights.groups)
                ul.li().text(group.name);
            body.br();
        }

        if (!session.user.rights.isEmpty()) {
            body.hr();
            body.h3().text(LUser.ROLES.toString() + ":");
            Ul ul = body.ul();
            for (UserRight r : session.user.rights)
                ul.li().text(r.name);
            body.br();
        }

        /* body.hr();
         body.h4().text("Zdarzenia");
         List<UserEvent> events = session.user.getEvents();
         Ul ul = body.ul();
         for (UserEvent ev : events)
         ul.li().text(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
         .format(ev.date) + ": " + ev.name + ", " + ev.value); */
    }
}

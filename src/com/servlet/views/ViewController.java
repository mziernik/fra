package com.servlet.views;

import com.servlet.views.connection.ViewConnection;
import com.context.index.Index;
import com.exceptions.*;
import com.html.js.JsActions;
import com.html.js.core.JsAction;
import com.json.JObject;
import com.lang.LContext;
import com.lang.LController;
import com.lang.LServlet;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.servlet.Handlers;
import com.servlet.controller.ControllerMetaData;
import com.utils.date.TDate;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import javax.websocket.CloseReason;

/**
 * WebSocket lub Ajax
 *
 * @author Miłosz Ziernik
 * @date 27 października 2015
 * @encoding UTF-8
 */
public abstract class ViewController {

    public final ViewOptions options = new ViewOptions(this);
    final TDate created = new TDate();
    long lastPeak = System.currentTimeMillis();
    public final ControllerMetaData httpController;
    public final ViewConnection connection = new ViewConnection(this);

    public final ViewControllerMeta meta;

    public ViewController() {
        meta = ViewControllerMeta.get(getClass());
        synchronized (ViewsManager.views) {
            ViewsManager.views.add(this);
        }

        ControllerMetaData controller = null;
        for (ControllerMetaData cd : Index.controllers)

            if (cd.endpoint().view() == getClass()) {
                controller = cd;
                break;
            }

        if (controller == null)
            throw new CoreException(LServlet.VIEW_CONTROLLER_NOT_FOUND.toString(getClass().getName()));

        this.httpController = controller;
    }

    public void broadcast(JsAction... actions) {
        for (ViewController ctrl : ViewsManager.get(getClass()))
            ctrl.push(actions);
    }

    public void push(JsAction... actions) {
        JObject json = new JObject();
        json.options.quotaNames(true)
                .acceptNulls(true)
                .intent("")
                .singleLine(true);

        json.put("#eval#", new JsActions(actions).toString());
        connection.send(json.toString());
    }

    public void onJsError(String url, String title, String message, String[] stack) {

        Log log = new Log(LogKind.WARNING)
                .tag("JS")
                .tag(title)
                .value(message)
                .url(url);

        LinkedList<String> lst = new LinkedList<>();
        for (String s : stack)
            lst.add(s.trim());

        log.errorStack.add(lst);
        log.send();
    }

    public void onError(Throwable e) {
        Log.warning("WebSocket", e);
        ErrorMessage msg = EError.format(e);

        JObject json = new JObject();
        json.options.quotaNames(true)
                .acceptNulls(true)
                .intent("")
                .singleLine(true);

        json.arrayC("#exception#").addAll(msg.title, msg.message, msg.details);
        connection.send(json.toString());
    }

    public void onClose(CloseReason reason) {
        synchronized (ViewsManager.views) {
            ViewsManager.views.remove(this);
        }
    }

    void peak() {
        lastPeak = System.currentTimeMillis();
    }

    /**
     * Rozłącz
     */
    public void close() {
        try {
            connection.close();
        } catch (IOException ex) {
            onError(ex);
        }
    }

    @Override
    public String toString() {
        return "Widok: " + (httpController != null ? "\""
                + httpController.endpoint().title() + "\"" : "")
                + " " + getClass().getName();
    }

}

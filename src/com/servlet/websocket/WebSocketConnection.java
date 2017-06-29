package com.servlet.websocket;

import com.context.AppContext;
import com.utils.Is;
import com.context.index.Index;
import com.exceptions.http.Http404FileNotFoundException;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.servers.WebAppServer;
import com.service.status.ServiceMonitor;
import com.service.status.StatusGroup;
import com.service.status.StatusItem;
import com.servlet.controller.BaseSession;
import com.servlet.views.ViewControllerMeta;
import com.servlet.views.connection.ViewWebSocketController;
import com.thread.ThreadObject;
import com.utils.Url;
import com.utils.collections.Params;
import com.utils.collections.Strings;
import com.utils.reflections.TClass;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.*;
import javax.websocket.CloseReason;
import javax.websocket.CloseReason.CloseCode;
import javax.websocket.Endpoint;

public abstract class WebSocketConnection extends Endpoint {

    public InetSocketAddress remoteAddress;
    public Params headers = new Params().caseSensitive(false);
    public String userAgent;
    public BaseSession httpSession;
    public Url requestUrl;
    public WebSocketController controller;
    private final static List<WebSocketConnection> connections = new LinkedList<>();
    private final static StatusGroup STATUS = StatusGroup.SERVICE.group("ws", "Połączenia WebSocket");
    private StatusItem sts;
    private boolean connected;

    protected void create() throws Exception {
        ThreadObject.clear();

        AppContext.waitForContext();

        String path = requestUrl.path().toString("/");

        String ctxPath = WebAppServer.context.getContextPath();

        if (ctxPath != null && path.startsWith(ctxPath))
            path = path.substring(ctxPath.length());

        if (path.startsWith("/"))
            path = path.substring(1);

        ThreadObject.wsConn.set(this);

        BaseSession ses = null;

        String cookie = headers.get("Cookie");

        String sidName = BaseSession.getSessionCookieName();
        if (!Is.empty(cookie))
            for (String line : cookie.split(";")) {
                String[] lines = line.split("=");
                if (lines[0].trim().equals(sidName) && lines.length == 2) {
                    ses = BaseSession.get(lines[1].trim());
                    break;
                }

            }
        httpSession = ses;

        for (Map.Entry<Class<? extends WebSocketController>, WebSocketEndpoint> en
                : Index.webSockets.entrySet())
            if (en.getValue() != null && en.getValue().url().equals(path)) {
                controller = new TClass<>(en.getKey()).newInstance(null);
                break;
            }

        if (controller == null) {
            ViewControllerMeta view = ViewControllerMeta.get(path);
            if (view != null)
                controller = new ViewWebSocketController(view);
        }

        if (controller == null)
            throw new Http404FileNotFoundException(path);

        synchronized (connections) {
            connections.add(this);
        }

        String name = controller.getClass().getName();
        if (controller instanceof ViewWebSocketController) {
            ViewWebSocketController vc = (ViewWebSocketController) controller;
            name = vc.ctrl.getClass().getName();
        }

        Strings cInfo = new Strings().nonEmpty(true);
        cInfo.add(name);
        cInfo.add(remoteAddress.getHostString());
        cInfo.add("thread " + Thread.currentThread().getId());

        if (httpSession != null) {
            cInfo.add(httpSession.user.username);
            httpSession.webSocketConnections.add(this);
        }

        String connInfo = cInfo.toString(", ");

        sts = STATUS.itemStr(path, path).value(connInfo);

        new Log(LogKind.EVENT)
                .tag("WebSocket")
                .value("Rozpoczęcie sesji " + connInfo)
                .address(remoteAddress.getHostString())
                .user(httpSession != null ? httpSession.user.username : null)
                .send();

        connected = true;
    }

    public boolean isConnected() {
        return connected;
    }

    protected void onClose(CloseReason reason) {
        connected = false;
        if (httpSession != null)
            httpSession.webSocketConnections.remove(this);
        sts.remove();
        synchronized (connections) {
            connections.remove(this);
        }
    }

    public static <T extends WebSocketController> LinkedList<T> getControllers(Class<T> ctrl) {
        LinkedList<T> conns = new LinkedList<>();
        synchronized (connections) {
            for (WebSocketConnection conn : connections)
                if (ctrl.isAssignableFrom(conn.controller.getClass()))
                    conns.add((T) conn.controller);
        }

        return conns;
    }

    public static List<WebSocketConnection> getConnections() {
        return new LinkedList<>(connections);
    }

    public abstract void send(String message) throws IOException;

    public abstract void send(byte[] data) throws IOException;

    public abstract void close(CloseCode closeCode, String reasonPhrase) throws IOException;

    public abstract void close() throws IOException;
}

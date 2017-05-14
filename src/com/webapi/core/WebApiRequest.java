package com.webapi.core;

import com.exceptions.EError;
import com.exceptions.ErrorMessage;
import com.intf.runnable.Runnable1;
import com.intf.runnable.RunnableEx2;
import com.json.*;
import com.lang.core.Language;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.servlet.controller.BaseSession;
import com.servlet.requests.HttpRequest;
import com.servlet.websocket.WebSocketConnection;
import com.servlet.websocket.WebSocketController;
import com.thread.ThreadObject;
import com.utils.Url;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.*;
import com.utils.date.TDate;
import java.io.FileNotFoundException;
import java.util.*;

public class WebApiRequest {

    public final TDate created = new TDate();
    public final Params params = new Params();
    public Params headers = new Params();
    @Deprecated
    final JElement json;
    public final String id;
    public final String endpointName;
    public final String location;
    public final WebApiController controller;
    public final HttpRequest http;
    public final WebSocketController webSocket;
    private final WebSocketConnection wsConn;
    public final Map<String, Object> responseHeaders = new LinkedHashMap<>();
    public final Url url;
    public final Language language;
    public final WebApi instance;
    RunnableEx2<String, JElement> onEvent;

    public final BaseSession session;
    final JArray jMessages = new JArray();
    public final WebApiControllerMeta meta;
    JObject source;
    boolean cancelled;

    public boolean isCancelled() {
        return cancelled || (webSocket != null && !webSocket.connection.isConnected());
    }

    @Override
    public String toString() {
        return endpointName;
    }

    public void info(CharSequence value) {
        info(null, value, null);
    }

    public void info(CharSequence title, CharSequence value) {
        info(title, value, null);
    }

    public void info(CharSequence title, CharSequence value, CharSequence details) {
        jMessages.object()
                .put("title", title)
                .put("value", value)
                .put("details", details)
                .put("hint", false)
                .put("type", "info");
    }

    public void warning(CharSequence title, CharSequence value) {
        warning(title, value, null);
    }

    public void warning(CharSequence title, CharSequence value, CharSequence details) {
        jMessages.object()
                .put("title", title)
                .put("value", value)
                .put("details", details)
                .put("hint", false)
                .put("type", "warning");
    }

    public void error(String title, CharSequence value) {
        error(title, value, null);
    }

    public void error(Throwable exception) {
        ErrorMessage err = EError.format(exception);
        error(err.title, err.message, null);
    }

    public void error(CharSequence title, CharSequence value, CharSequence details) {
        jMessages.object()
                .put("title", title)
                .put("value", value)
                .put("details", details)
                .put("hint", false)
                .put("type", "error");
    }

    private boolean responseCommited;

    public WebApiRequest onEvent(RunnableEx2<String, JElement> onEvent) {
        this.onEvent = onEvent;
        return this;
    }

    public boolean isResponseCommited() {
        return responseCommited;
    }

    public void responseCommit(Object result) {
        controller.response(wsConn, http, this, result, null);
    }

    public void responseError(Throwable ex) {
        controller.response(wsConn, http, this, null, ex);
    }

    public void success(CharSequence value) {
        success(null, value, null);
    }

    public void success(CharSequence title, CharSequence value) {
        success(title, value, null);
    }

    public void success(CharSequence title, CharSequence value, CharSequence details) {
        jMessages.object()
                .put("title", title)
                .put("value", value)
                .put("details", details)
                .put("hint", false)
                .put("type", "success");
    }

    public void clearMessages() {
        jMessages.clear();
    }

    public void infoHint(CharSequence value) {
        jMessages.object()
                .put("value", value)
                .put("hint", true)
                .put("type", "info");
    }

    public void warningHint(CharSequence value) {
        jMessages.object()
                .put("value", value)
                .put("hint", true)
                .put("type", "warning");
    }

    public void errorHint(CharSequence value) {
        jMessages.object()
                .put("value", value)
                .put("hint", true)
                .put("type", "error");
    }

    public void successHint(CharSequence value) {
        jMessages.object()
                .put("value", value)
                .put("hint", true)
                .put("type", "success");
    }

    WebApiRequest(WebApiController controller, HttpRequest http,
            WebSocketController webSocket, Language language, Url url, String location,
            String endpointName, String requestId, JElement data,
            Runnable1<Params> fillParams) throws Exception {
        if (Is.empty(requestId))
            requestId = Utils.randomId();
        this.location = location;
        this.endpointName = endpointName;
        this.http = http;
        this.webSocket = webSocket;
        this.wsConn = webSocket != null ? webSocket.connection : null;
        this.controller = controller;
        this.url = url;
        this.id = requestId;
        this.json = data == null ? new JNull() : data;
        this.language = Objects.requireNonNull(language, "WebApiRequest Language");

        fillParams.run(this.params);
        Pair<WebApiControllerMeta, WebApi> endp = controller.getEndpoint(this, endpointName);

        if (endp == null)
            throw new FileNotFoundException(endpointName);

        this.meta = endp.first;
        this.instance = endp.second;
        this.session = controller.session;

        controller.requests.put(requestId, this);
        ThreadObject.webApiReq.set(this);
        ThreadObject.language.set(language);
    }

    @Deprecated
    public JElement getJson() {
        if (json == null || json.isNull())
            throw new Error("Missing JSON data");
        return json;
    }

    public void eventInfo(String title, String message) {
        if (webSocket == null)
            return;

        JObject obj = controller.buildEvent(this, null, "hint", null);
        obj.arrayC("messages").object()
                .put("title", title)
                .put("value", message)
                .put("hint", true)
                .put("type", "info");

        logEvent(this, obj);
        webSocket.send(obj.toString());
    }

    private void logEvent(WebApiRequest req, JObject json) {
        json.options.compactMode(false);
        Log log = new Log(LogKind.DEBUG)
                .tag("WebApi", "Event")
                .details(json.toString())
                .value(req.endpointName + ", " + json.getStr("event", ""));
        log.send();
    }

    public void event(String source, String name, JElement data) {
        if (webSocket == null)
            return;
        webSocket.send(controller.buildEvent(this, source, name, data).toString());
    }

    public enum WebApiMessageType {
        INFO,
        WARNING,
        ERROR
    }

}

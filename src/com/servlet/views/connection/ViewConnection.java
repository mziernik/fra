package com.servlet.views.connection;

import com.servlet.controller.BaseSession;
import com.servlet.views.ViewController;
import com.thread.ThreadObject;
import com.utils.Url;
import com.utils.collections.MapList;
import com.utils.collections.Params;
import java.io.IOException;

public class ViewConnection {

    public final Params headers = new Params().caseSensitive(false);
    public final Params params = new Params();
    public String remoteAddress;
    final ViewWebSocketController webSocket;
    private final ViewController ctrl;
    public final Url requestUrl;
    public final BaseSession httpSession;

    public boolean isWebSocket() {
        return webSocket != null;
    }

    public ViewConnection(ViewController controller) {
        ctrl = controller;
        webSocket = ThreadObject.viewWebSocketController.removeF();
        headers.addAll(webSocket.connection.headers);
        params.addAll(webSocket.connection.requestUrl.params());
        remoteAddress = webSocket.connection.remoteAddress.getHostString();
        requestUrl = webSocket.connection.requestUrl;
        httpSession = webSocket.connection.httpSession;
    }

    public void send(String message) {
        if (webSocket != null)
            webSocket.send(message);
    }

    public void close() throws IOException {
        if (webSocket != null)
            webSocket.close();
    }

}

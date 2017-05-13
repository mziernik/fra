package com.fx.webkit;

import com.utils.console.TConsole;
import com.mlogger.Log;
import com.servers.websocket.WebSocket;
import com.servers.websocket.handshake.ClientHandshake;
import com.servers.websocket.server.WebSocketServer;
import com.sun.javafx.scene.web.Debugger;
import java.net.InetSocketAddress;
import java.util.LinkedList;
import javafx.application.Platform;
import javafx.util.Callback;

public class DevToolsServer extends WebSocketServer implements Callback<String, Void> {

    private final Debugger debugger;
    private WebSocket conn;

    public DevToolsServer(Debugger debugger) throws Exception {
        super(new InetSocketAddress("127.0.0.1", 51742));
        this.debugger = debugger;
        start();

        debugger.setEnabled(true);
        debugger.sendMessage("{\"id\" : -1, \"method\" : \"Network.enable\"}");
        debugger.setMessageCallback(this);

        TConsole.printTs("[DevTools]: chrome-devtools://devtools/bundled/inspector.html?ws=localhost:51742");
        Log.info("DevTools", "URL: chrome-devtools://devtools/bundled/inspector.html?ws=localhost:51742");
    }

    private final LinkedList<String> sendQueue = new LinkedList<>();

    @Override
    public void onOpen(WebSocket conn, ClientHandshake handshake) {
        this.conn = conn;
        Log.debug("DevTools", "Połączone");

        synchronized (sendQueue) {
            for (String s : sendQueue)
                conn.send(s);
            sendQueue.clear();
        }
    }

    @Override
    public void onClose(WebSocket conn, int code, String reason, boolean remote) {
        Log.debug("DevTools", "Rozłączone");
    }

    @Override
    public void onMessage(WebSocket conn, String message) {
        Platform.runLater(() -> {
            debugger.sendMessage(message);
        });
    }

    @Override
    public void onError(WebSocket conn, Exception ex) {
        Log.error(ex);
    }

    @Override
    public Void call(String data) {
        if (conn == null)
            synchronized (sendQueue) {
                sendQueue.add(data);
                while (sendQueue.size() > 1000)
                    sendQueue.pollFirst();
            }
        else
            conn.send(data);

        return null;
    }
}

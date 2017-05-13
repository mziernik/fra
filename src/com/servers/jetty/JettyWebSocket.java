package com.servers.jetty;

import com.exceptions.EError;
import com.mlogger.Log;
import com.servlet.websocket.WebSocketConnection;
import com.thread.ThreadObject;
import com.utils.Url;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.websocket.CloseReason;
import javax.websocket.CloseReason.CloseCode;
import javax.websocket.CloseReason.CloseCodes;
import javax.websocket.EndpointConfig;
import org.eclipse.jetty.websocket.api.*;
import org.eclipse.jetty.websocket.api.annotations.*;
import org.eclipse.jetty.websocket.common.WebSocketSession;

@WebSocket
public class JettyWebSocket
        extends WebSocketConnection
        implements WebSocketListener {

    private WebSocketSession session;
    private RemoteEndpoint remote;

    @Override
    public void onWebSocketConnect(Session session) {
        try {
            session.setIdleTimeout(0);
            UpgradeRequest req = session.getUpgradeRequest();

            this.session = (WebSocketSession) session;
            remoteAddress = session.getRemoteAddress();
            req.getHeader("User-Agent");
            headers.addAll(req.getHeaders());

            requestUrl = new Url(req.getRequestURI().toString());
            remote = session.getRemote();
            create();
        } catch (Throwable e) {
            Log.error(e);
            session.close(CloseCodes.PROTOCOL_ERROR.getCode(), EError.format(e).message);
        }
    }

    @Override
    public void onOpen(javax.websocket.Session sn, EndpointConfig ec) {

    }

    @Override
    public void send(String message) throws IOException {
        if (session.isOpen())
            remote.sendString(message);
    }

    @Override
    public void send(byte[] data) throws IOException {
        if (session.isOpen())
            remote.sendBytes(ByteBuffer.wrap(data));

    }

    @Override
    public void close(CloseCode closeCode, String reasonPhrase) throws IOException {
        session.close(closeCode.getCode(), reasonPhrase);
    }

    @Override
    public void close() throws IOException {
        session.close();
    }

    @Override
    public void onWebSocketBinary(byte[] bytes, int offset, int length) {
        if (session.isOpen())
            controller.onMessage(ByteBuffer.wrap(bytes, offset, length).array());
    }

    @Override
    public void onWebSocketText(String string) {
        if (session.isOpen())
            controller.onMessage(string);
    }

    @Override
    public void onWebSocketClose(int code, String message) {
        CloseReason reason = new CloseReason(CloseCodes.getCloseCode(code), message);
        super.onClose(reason);
        controller.onClose(reason);
        ThreadObject.clear();
    }

    @Override
    public void onWebSocketError(Throwable ex) {
        if (controller != null)
            controller.onError(ex);
        else
            Log.error(ex);
    }

}

package com.servlet.websocket;

import com.exceptions.EError;
import com.mlogger.Log;
import com.servlet.websocket.MainWebSocketJsr.MainWebSocketConfigurator;
import com.utils.Url;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;
import javax.servlet.http.*;
import javax.websocket.*;
import javax.websocket.CloseReason.CloseCode;
import javax.websocket.CloseReason.CloseCodes;
import javax.websocket.MessageHandler.Whole;
import javax.websocket.RemoteEndpoint.Basic;
import javax.websocket.server.*;
import javax.websocket.server.ServerEndpointConfig.Configurator;

/**
 * Miłosz Ziernik 2014/06/19
 */
@ServerEndpoint(value = "/", configurator = MainWebSocketConfigurator.class)
public class MainWebSocketJsr extends WebSocketConnection {

    private Session session;
    private EndpointConfig config;
    Basic remote;
    private boolean closed;

    //  WebSocketPage webSocket;
    @Override
    @OnOpen
    public void onOpen(Session session, EndpointConfig config) {
        try {
            this.session = session;
            this.config = config;
            remote = session.getBasicRemote();

            remote.setBatchingAllowed(true);

            // nie zamieniać na lambdę
            session.addMessageHandler(new Whole<String>() {

                @Override
                public void onMessage(String message) {
                    controller.onMessage(message);
                }
            });

            // nie zamieniać na lambdę
            session.addMessageHandler(new Whole<ByteBuffer>() {

                @Override
                public void onMessage(ByteBuffer data) {
                    controller.onMessage(data.array());
                }
            });

            Map<String, Object> props = config.getUserProperties();

            Map<String, List<String>> hdrs = (Map<String, List<String>>) props.get("$headers");
            Map<String, List<String>> pars = (Map<String, List<String>>) props.get("$params");

            remoteAddress = (InetSocketAddress) props.get("$addr");
            requestUrl = new Url((String) props.get("$url"));

            headers.addAll((Map) hdrs);

            create();

        } catch (Throwable e) {
            try {
                Log.error(e);
                session.close(new CloseReason(CloseCodes.PROTOCOL_ERROR, EError.format(e).message));
            } catch (IOException ex) {
                Log.error(ex);
            }
        }
    }

    @Override
    public void onClose(Session sn, CloseReason cr) {
        closed = true;
        super.onClose(sn, cr);
        if (controller != null)
            controller.onClose(cr);
    }

    @Override
    public void onError(Session sn, Throwable thrwbl) {
        super.onError(sn, thrwbl);
        if (controller != null)
            controller.onError(thrwbl);
    }

    @Override
    public void send(String message) throws IOException {
        if (!closed)
            try {
                remote.sendText(message);
            } catch (java.lang.IllegalStateException e) {
                controller.onError(e);
            }
    }

    @Override
    public void send(byte[] data) throws IOException {
        if (!closed)
            remote.sendBinary(ByteBuffer.wrap(data));
    }

    @Override
    public void close(CloseCode closeCode, String reasonPhrase) throws IOException {
        closed = true;
        session.close(new CloseReason(closeCode, reasonPhrase));
    }

    @Override
    public void close() throws IOException {
        closed = true;
        session.close();
    }

    public static class MainWebSocketConfigurator extends Configurator {

        @Override
        public void modifyHandshake(ServerEndpointConfig config,
                HandshakeRequest request, HandshakeResponse response) {
            if (request == null)
                return;

            Map<String, Object> props = config.getUserProperties();

            try {
                Field f = request.getClass().getDeclaredField("request");

                if (f != null) {
                    f.setAccessible(true);
                    Object obj = f.get(request);
                    if (obj instanceof HttpServletRequest) {
                        HttpServletRequest hreq = (HttpServletRequest) obj;
                        props.put("$addr", InetSocketAddress.createUnresolved(
                                hreq.getRemoteHost(), hreq.getRemotePort()));
                        props.put("$url", hreq.getRequestURL().toString());
                    }
                }

            } catch (Throwable e) {
                Log.warning(e);
            }

            props.put("$headers", request.getHeaders());
            props.put("$params", request.getParameterMap());

        }
    }

}

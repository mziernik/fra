package com.servers.websocket.server;

import java.net.Socket;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.util.List;

import com.servers.websocket.WebSocketAdapter;
import com.servers.websocket.WebSocketImpl;
import com.servers.websocket.drafts.Draft;
import com.servers.websocket.server.WebSocketServer.WebSocketServerFactory;

public class DefaultWebSocketServerFactory implements WebSocketServerFactory {

    @Override
    public WebSocketImpl createWebSocket(WebSocketAdapter a, Draft d, Socket s) {
        return new WebSocketImpl(a, d);
    }

    @Override
    public WebSocketImpl createWebSocket(WebSocketAdapter a, List<Draft> d, Socket s) {
        return new WebSocketImpl(a, d);
    }

    @Override
    public SocketChannel wrapChannel(SocketChannel channel, SelectionKey key) {
        return (SocketChannel) channel;
    }
}

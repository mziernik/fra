package com.net.tcp;

import java.net.Socket;

public abstract class TcpClientSocket extends TcpClient {

    final TcpServer<TcpClientSocket> server;

    public TcpClientSocket(TcpServer<TcpClientSocket> server, Socket socket) {
        super(server, socket);
        this.server = server;

    }

    protected abstract void execute() throws Exception;

    protected TcpServer<?> getServer() {
        return server;
    }

}

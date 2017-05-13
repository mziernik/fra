package com.mlogger.handlers;

import com.utils.Utils;
import com.utils.Is;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import com.mlogger.LogElement;
import com.mlogger.utils._Internal;

public class TcpHandler extends LogHandler {

    public final SocketAddress address;
    private Socket socket;

    public int retryCount = 1;
    public int connectTimeout = 2000;
    public int readTimeout = 5000;

    public TcpHandler(SocketAddress address) {
        super();
        this.address = address;
    }

    @Override
    public boolean equals(Object o) {
        return (o instanceof TcpHandler) && address.equals(((TcpHandler) o).address);
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 89 * hash + (this.address != null ? this.address.hashCode() : 0);
        return hash;
    }

    @Override
    public String toString() {
        return "tcp://" + address.toString().substring(1);
    }

    private BufferedReader reader;
    private InputStream in;
    private OutputStream out;

    @Override
    public void publish(LogElement log, LinkedList<Handler> handlers, LogRecord record) throws Exception {

        byte[] data = prepareData(log);

        for (int i = 0; i < (retryCount >= 0 ? retryCount : 0) + 1; i++)
            try {
                if (socket == null || !socket.isConnected()) {
                    socket = new Socket();
                    socket.setSoTimeout(readTimeout);
                    socket.connect(address, connectTimeout);
                    in = socket.getInputStream();
                    reader = new BufferedReader(new InputStreamReader(in, Utils.UTF8));
                    out = socket.getOutputStream();
                }

                int avail = in.available();
                if (avail > 0) {
                    byte[] buff = new byte[avail];
                    in.read(buff);
                }

                out.write(data);
                out.flush();

                if (log.options.priority)
                    try {
                        String line = reader.readLine();
                        processResponse(log, line);
                    } catch (SocketTimeoutException e) {
                        throw new ServerException("Przekroczony limit czasu "
                                + "oczekiwania na potwierdzenie ze strony serwera");
                    }

                return;

            } catch (Exception e) {
                socket.close();
                if (retryCount <= 0 || i == retryCount)
                    _Internal.onException(e, this, log);
                socket = null;
            }
    }

    @Override
    public void close() throws SecurityException {
        if (socket != null)
            try {
                socket.close();
            } catch (IOException ex) {
            }
        socket = null;
        super.close();
    }

}

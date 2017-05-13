package com.mlogger.handlers;

import com.exceptions.EError;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import com.mlogger.LogElement;
import java.net.*;

public class UdpHandler extends LogHandler {

    public final SocketAddress address;
    private DatagramSocket socket;

    public UdpHandler(SocketAddress address) {
        super();
        this.address = address;
    }

    @Override
    public boolean equals(Object o) {
        return (o instanceof UdpHandler) && address.equals(((UdpHandler) o).address);
    }

    @Override
    public String toString() {
        return "udp://" + address.toString().substring(1);
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 53 * hash + (this.address != null ? this.address.hashCode() : 0);
        return hash;
    }

    @Override
    public void publish(LogElement log, LinkedList<Handler> handlers, LogRecord record) throws Exception {

        byte[] data = prepareData(log);

        if (socket == null)
            socket = new DatagramSocket();
        try {
            socket.send(new DatagramPacket(data, data.length, address));
        } catch (SocketException e) {
            throw EError.processSocketException(e, address);
        }
    }

    @Override
    public void close() throws SecurityException {
        if (socket != null)
            socket.close();
        socket = null;
        super.close();
    }

}

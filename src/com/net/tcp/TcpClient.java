package com.net.tcp;

import java.io.*;
import java.net.*;
import java.nio.channels.SocketChannel;

public class TcpClient extends Socket {

    private final Socket socket;

    private SocketOutputStream output;
    private SocketInputStream input;
    final SocketNOP nop;
    private String name;
    private final TcpServer<?> server;
    private static int OUTPUT_BUFFER_SIZE = 1492;

    public TcpClient() {
        this(null, new Socket());
    }

    public TcpClient(TcpServer<?> server, Socket socket) {
        this.socket = socket;
        this.server = server;
        this.nop = new SocketNOP(this);
        updateName();
    }

    public String getRemoteAddress() {
        return socket.getRemoteSocketAddress() != null
                ? socket.getRemoteSocketAddress().toString().substring(1)
                : null;
    }

    private void updateName() {
        name = "";
        if (server != null)
            name = server.name + " " + server.server.getInetAddress().toString()
                    + " [" + getRemoteAddress() + "]";
        
        name += "TCP " + socket.getLocalAddress().toString().substring(1)
                + " -> " + getRemoteAddress();
    }

    public String getName() {
        return name;
    }

    public synchronized void write(byte[] data) throws IOException {
        if (output == null)
            return;
        output.write(data);
        output.flush();
    }

    public void setNOP(int interval) throws SocketException {
        nop.setInterval(interval);
        if (nop.getState() == Thread.State.NEW)
            nop.start();
    }

    protected void onNOP() {

    }

    //VT100
    protected boolean accept() {
        return true;
    }

    public boolean isRunning() {
        return socket.isConnected()
                && !socket.isClosed()
                && socket.isBound()
                && !socket.isInputShutdown()
                && !socket.isOutputShutdown();
    }

    @Override
    public void close() throws IOException {
        socket.close();
    }

    @Override
    public void bind(SocketAddress bindpoint) throws IOException {
        socket.bind(bindpoint);
    }

    @Override
    public void connect(SocketAddress endpoint) throws IOException {
        socket.connect(endpoint);
        updateName();
    }

    @Override
    public void connect(SocketAddress endpoint, int timeout) throws IOException {
        socket.connect(endpoint, timeout);
        updateName();
    }

    @Override
    public SocketChannel getChannel() {
        return socket.getChannel();
    }

    @Override
    public InetAddress getInetAddress() {
        return socket.getInetAddress();
    }

    @Override
    public InputStream getInputStream() throws IOException {
        if (input == null)
            input = new SocketInputStream(this, socket.getInputStream());
        return input;
    }

    @Override
    public boolean getKeepAlive() throws SocketException {
        return socket.getKeepAlive();
    }

    @Override
    public InetAddress getLocalAddress() {
        return socket.getLocalAddress();
    }

    @Override
    public SocketAddress getLocalSocketAddress() {
        return socket.getLocalSocketAddress();
    }

    @Override
    public boolean getOOBInline() throws SocketException {
        return socket.getOOBInline();
    }

    @Override
    public void shutdownOutput() throws IOException {
        socket.shutdownOutput();
    }

    @Override
    public void shutdownInput() throws IOException {
        socket.shutdownInput();
    }

    @Override
    public void setTrafficClass(int tc) throws SocketException {
        socket.setTrafficClass(tc);
    }

    @Override
    public void setTcpNoDelay(boolean on) throws SocketException {
        socket.setTcpNoDelay(on);
    }

    @Override
    public synchronized void setSoTimeout(int timeout) throws SocketException {
        socket.setSoTimeout(timeout);
    }

    @Override
    public void setReuseAddress(boolean on) throws SocketException {
        socket.setReuseAddress(on);
    }

    @Override
    public synchronized int getReceiveBufferSize() throws SocketException {
        return socket.getReceiveBufferSize();
    }

    @Override
    public OutputStream getOutputStream() throws IOException {
        if (output == null)
            output = new SocketOutputStream(this, new BufferedOutputStream(
                    socket.getOutputStream(), OUTPUT_BUFFER_SIZE));
        return output;
    }

    @Override
    public boolean isBound() {
        return socket.isBound();
    }

    @Override
    public int getPort() {
        return socket.getPort();
    }

    @Override
    public void sendUrgentData(int data) throws IOException {
        socket.sendUrgentData(data);
    }

    @Override
    public boolean isClosed() {
        return socket.isClosed();
    }

    @Override
    public void setOOBInline(boolean on) throws SocketException {
        socket.setOOBInline(on);
    }

    @Override
    public synchronized void setReceiveBufferSize(int size) throws SocketException {
        socket.setReceiveBufferSize(size);
    }

    @Override
    public void setSoLinger(boolean on, int linger) throws SocketException {
        socket.setSoLinger(on, linger);
    }

    @Override
    public void setPerformancePreferences(int connectionTime, int latency, int bandwidth) {
        socket.setPerformancePreferences(connectionTime, latency, bandwidth);
    }

    @Override
    public synchronized void setSendBufferSize(int size) throws SocketException {
        socket.setSendBufferSize(size);
    }

    @Override
    public void setKeepAlive(boolean on) throws SocketException {
        socket.setKeepAlive(on);
    }

    @Override
    public boolean isOutputShutdown() {
        return socket.isOutputShutdown();
    }

    @Override
    public boolean isInputShutdown() {
        return socket.isInputShutdown();
    }

    @Override
    public boolean isConnected() {
        return socket.isConnected();
    }

    @Override
    public int getTrafficClass() throws SocketException {
        return socket.getTrafficClass();
    }

    @Override
    public boolean getTcpNoDelay() throws SocketException {
        return socket.getTcpNoDelay();
    }

    @Override
    public SocketAddress getRemoteSocketAddress() {
        return socket.getRemoteSocketAddress();
    }

    @Override
    public synchronized int getSendBufferSize() throws SocketException {
        return socket.getSendBufferSize();
    }

    @Override
    public synchronized int getSoTimeout() throws SocketException {
        return socket.getSoTimeout();
    }

    @Override
    public int getSoLinger() throws SocketException {
        return socket.getSoLinger();
    }

    @Override
    public int getLocalPort() {
        return socket.getLocalPort();
    }

    @Override
    public boolean getReuseAddress() throws SocketException {
        return socket.getReuseAddress();
    }
}

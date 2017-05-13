package com.net.tcp;

import java.io.IOException;
import java.io.InputStream;

public class SocketInputStream extends InputStream {

    private final InputStream in;
    private final TcpClient socket;

    public SocketInputStream(TcpClient socket, InputStream in) {
        this.socket = socket;
        this.in = in;
    }

    @Override
    public int read() throws IOException {
        return in.read();
    }

    @Override
    public int available() throws IOException {
        return in.available();
    }

    @Override
    public void close() throws IOException {
        in.close();
    }

    @Override
    public synchronized void reset() throws IOException {
        in.reset();
    }

    @Override
    public long skip(long n) throws IOException {
        return in.skip(n);
    }

    @Override
    public synchronized void mark(int readlimit) {
        in.mark(readlimit);
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        return in.read(b, off, len);
    }

    @Override
    public int read(byte[] b) throws IOException {
        return in.read(b);
    }

    @Override
    public boolean markSupported() {
        return in.markSupported();
    }

}

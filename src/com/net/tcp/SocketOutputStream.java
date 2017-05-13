package com.net.tcp;

import java.io.IOException;
import java.io.OutputStream;

public class SocketOutputStream extends OutputStream {

    private final OutputStream out;
    private final TcpClient socket;

    private Thread lock;
    private final Object lockNotify = new Object();

    public SocketOutputStream(TcpClient socket, OutputStream out) {
        this.socket = socket;
        this.out = out;
    }

    public void lock() {
        lock = Thread.currentThread();
    }

    public Thread getLock() {
        return lock;
    }

    public void unlock() {
        lock = null;
        synchronized (lockNotify) {
            lockNotify.notifyAll();
        }
    }

    private void checkLock() throws IOException {
        if (lock == null || lock.equals(Thread.currentThread()))
            return;

        synchronized (lockNotify) {
            try {
                lockNotify.wait();
            } catch (InterruptedException ex) {
                throw new IOException(ex);
            }
        }

    }

    @Override
    public void write(int b) throws IOException {
        checkLock();
        out.write(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
        checkLock();
        lock();
        try {
            super.write(b);
        } finally {
            unlock();
        };
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        checkLock();
        lock();
        try {
            super.write(b, off, len);
        } finally {
            unlock();
        };

    }

    @Override
    public void flush() throws IOException {
        socket.nop.reset();
        out.flush();
    }

    @Override
    public void close() throws IOException {
        socket.nop.reset();
        out.close();
    }
}

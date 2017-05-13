package com.net.tcp;

public class SocketNOP extends Thread {

    private final TcpClient socket;
    private final Object resetNotify = new Object();
    private boolean resetCalled;
    private int interval = 60000;

    public SocketNOP(TcpClient socket) {
        this.socket = socket;
        setName(socket.getName() + " NOP");
        setPriority(MIN_PRIORITY);
    }

    void reset() {
        synchronized (resetNotify) {
            resetCalled = true;
            resetNotify.notify();
        }
    }

    @Override
    public void run() {
        while (socket.isRunning())
            try {
                synchronized (resetNotify) {
                    resetCalled = false;
                    if (interval <= 0)
                        resetNotify.wait();
                    else
                        resetNotify.wait(interval);

                    if (resetCalled) {
                        resetCalled = false;
                        continue;
                    }
                }

                if (socket.isRunning())
                    socket.onNOP();

            } catch (InterruptedException e) {
                return;
            }

    }

    void setInterval(int interval) {
        this.interval = interval;
        reset();
    }

}

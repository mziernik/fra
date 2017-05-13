package com.thread;

public abstract class Timeout {

    private final TThread thread;
    private boolean reset = true;

    protected abstract void run() throws Exception;

    public Timeout reset() {
        synchronized (thread) {
            reset = true;
            thread.notify();
        }
        return this;
    }

    public Timeout clear() {
        synchronized (thread) {
            thread.interrupt();
            thread.notify();
        }
        return this;
    }

    public Timeout(String name, final int timeout) {
        thread = new TThread(name) {

            @Override
            protected void run() throws Exception {

                while (reset)
                    synchronized (thread) {
                        reset = false;
                        thread.wait(timeout);
                    }

                if (!isRunning())
                    return;

                run();
            }
        };

        thread.start();

    }

}

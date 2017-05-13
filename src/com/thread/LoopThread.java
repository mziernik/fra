package com.thread;

/**
 * Miłosz Ziernik 2013/11/01
 */
public abstract class LoopThread extends TThread {

    public int delay = 1000;
    public boolean constantInterval = false;

    /**
     * Metoda zwraca czas wstrzymania. Jeśli wartość jest mniejsza od 0, wątek
     * zostanie przerwany
     */
    protected abstract void loop() throws Exception;

    public LoopThread(String name, int delay) {
        super(name);
        this.delay = delay;
    }

    protected boolean onBeforeRun() {
        return true;
    }

    protected void onDone() {

    }

    /*
     protected boolean onException(Exception e) {
     return true;
     }
     */
    @Override
    public void run() throws Exception {

        if (!onBeforeRun())
            return;

        try {

            long time = System.currentTimeMillis();

            while (isRunning())
                try {

                    long sleep = delay;

                    if (constantInterval)
                        sleep = delay - (System.currentTimeMillis() - time);

                    if (sleep < 0)
                        sleep = 0;
                    Thread.sleep(sleep);

                    time = System.currentTimeMillis();

                    loop();

                } catch (InterruptedException ex) {
                    return;
                } catch (Throwable e) {
                    onException(e);
                }

        } finally {
            onDone();
        }
    }
}

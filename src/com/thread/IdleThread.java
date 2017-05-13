package com.thread;

import com.mlogger.Log;
import java.util.Date;

/**
 * Miłosz Ziernik 2013/11/07
 */
public abstract class IdleThread extends TThread {

    private final Object notify = new Object();
    private int idleTime = 10000;
    private int maxPeaks = 0;
    private boolean peak = false;
    private int counter;

    public void peak() {
        synchronized (notify) {
            peak = true;
            ++counter;
            notify.notify();
        }
    }

    public IdleThread(String name, int idleTime, int maxPeaks) {
        super(name);
        this.idleTime = idleTime;
        this.maxPeaks = maxPeaks;
    }

    protected abstract void onIdle(IdleThread thread) throws Exception;

    @Override
    protected void run() throws Exception {
        while (isRunning()) {

            long time = new Date().getTime();

            synchronized (notify) {
                notify.wait(idleTime);
            }

            if ((maxPeaks > 0 && counter > maxPeaks)
                    || (peak && new Date().getTime() - time > idleTime - 10))
                try {
                    peak = false;
                    counter = 0;
                    onIdle(this);
                } catch (Throwable e) {
                    Log.error(e);
                }
        }
    }

}

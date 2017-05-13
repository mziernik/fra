package com.thread;

import com.mlogger.Log;
import com.context.AppContext;
import com.intf.runnable.RunnableEx1;
import java.lang.Thread.State;
import java.util.*;

/**
 * Miłosz Ziernik 2013/11/01
 */
public abstract class TThread {

    private ThreadDump dump;
    private final TThreadRunnable thread;

    public static TThread create(String name, RunnableEx1<TThread> runnable) {
        return new TThread(name) {
            @Override
            protected void run() throws Exception {
                runnable.run(this);
            }
        };
    }

    public static TThread create(String name, RunnableEx1<TThread> runnable,
            RunnableEx1<Throwable> onExceprion) {
        return new TThread(name) {
            @Override
            protected void run() throws Exception {
                try {
                    runnable.run(this);
                } catch (Throwable e) {
                    onExceprion.run(e);
                }
            }
        };
    }

    private class TThreadRunnable extends Thread {

        public TThreadRunnable(ThreadGroup group, String name) {
            super(group, name);
        }

        @Override
        public void run() {

            if (dump != null)
                ThreadObject.parentThread.set(this, dump);

            if (delay > 0)
                try {
                    Thread.sleep(delay);

                } catch (InterruptedException ex) {
                    return;
                }

            String tname = null;

            synchronized (allThreads) {
                allThreads.add(TThread.this);
            }
            Throwable err = null;
            try {
                try {
                    tname = getName();
                    if (tname == null)
                        tname = "TThread " + getId();
                    if (!tname.contains(Long.toString(getId())))
                        tname += " [" + getId() + "]";
                    if (AppContext.devMode)
                        Log.event("Thread", "Start: " + tname,
                                "class: " + getClass().getName());

                    TThread.this.run();
                } catch (InterruptedException ex) {
                    err = ex;
                    return;
                } catch (Throwable ex) {
                    err = ex;
                    try {
                        onException(ex);
                    } catch (Throwable ee) {
                        err = ee;
                        return;
                    }
                }
            } finally {
                onTerminate(err);
                if (AppContext.devMode)
                    Log.event("Thread", "Stop: " + tname,
                            "class: " + getClass().getName());
            }
            synchronized (allThreads) {
                allThreads.remove(this);
            }

        }
    }

    protected int delay = 0;
    private final static List<TThread> allThreads = new LinkedList<>();

    public static List<TThread> getAllThreads() {
        List<TThread> list = new LinkedList<>();
        synchronized (allThreads) {
            list.addAll(allThreads);
        }
        return list;
    }

    public TThread(final String name) {
        this.thread = new TThreadRunnable(null, name);
    }

    public TThread(ThreadGroup group, final String name) {
        this.thread = new TThreadRunnable(group, name);
    }

    protected abstract void run() throws Exception;

    protected void onTerminate(Throwable exception) {
    }

    protected void onException(Throwable e) {
        Log.error(e);
    }

    /**
     * Podstawowa metoda sprawdzania czy można kontynuować iterację pętli wątku
     *
     * @return
     */
    public boolean isRunning() {
        return thread.isAlive() && !thread.isInterrupted();
    }

    public TThread interupt(int wait) {
        thread.interrupt();
        if (wait > 0)
            try {
                thread.join(wait);
            } catch (InterruptedException ex) {
                return this;
            }

        return this;
    }

    public static void interuptAll(int timeout) throws InterruptedException {
        List<TThread> list = new LinkedList<>();
        synchronized (TThread.allThreads) {
            for (TThread th : TThread.allThreads) {
                list.add(th);
                th.interrupt();
            }
        }
        if (timeout > 0)
            for (TThread th : list)
                th.join(timeout / list.size());

    }

    public TThread interrupt() {
        thread.interrupt();
        return this;
    }

    public TThread join(int millis) throws InterruptedException {
        thread.join(millis);
        return this;
    }

    public TThread join() throws InterruptedException {
        thread.join();
        return this;
    }

    /**
     * Uruchamia wątek. Można wywoływać wielokrotnie
     *
     * @return
     */
    public synchronized TThread start() {
        if (thread.getState() == State.NEW)
            try {
                dump = new ThreadDump();
                // 
                if (thread.getState() == State.NEW)
                    thread.start();
            } catch (IllegalThreadStateException e) {
                Log.warning(e).comment("State: " + thread.getState());
            }
        return this;
    }

    public synchronized TThread start(int delay) {
        this.delay = delay;
        if (thread.getState() == State.NEW)
            try {
                dump = new ThreadDump();
                thread.start();
            } catch (IllegalThreadStateException e) {
                Log.warning(e).comment("State: " + thread.getState());
            }
        return this;
    }

    public TThread setPriority(int priority) {
        thread.setPriority(priority);
        return this;
    }

    public TThread setDaemon(boolean daemon) {
        thread.setDaemon(daemon);
        return this;
    }

    public TThread setName(String name) {
        thread.setName(name);
        return this;
    }

    public boolean isInterrupted() {
        return thread.isInterrupted();
    }
}

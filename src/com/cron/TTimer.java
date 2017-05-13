package com.cron;

import com.context.AppContext;
import com.intf.runnable.RunnableEx1;
import com.utils.Utils;
import com.utils.Is;
import com.mlogger.Log;
import com.thread.TThread;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 04 grudnia 2015
 * @encoding UTF-8 Klasa, której metoda run() wywoływana jest cyklicznie co
 * określony czas. Wszystko bazuje na jednym wątku, także należy zwracać uwagę,
 * aby kod metody run() nie wykonywał się zbyt długo. Timer nie jest precyzyjny,
 * przeznaczony jest do cyklicznych zadań, których interwał nie jest krytyczny,
 * np funkcje porządkujące
 */
public abstract class TTimer {

    protected int interval = 1000; // minimalny interwał zdarzeń 
    protected long lastRunnedTS = System.currentTimeMillis();
    protected int runCounter = 0;
    protected final Map<String, Object> extra = new HashMap<>();

    public static TTimer instance(Interval interval, RunnableEx1<TTimer> runnable) {

        return new TTimer(interval) {
            @Override
            protected void run() throws Exception {
                runnable.run(this);
            }
        };

    }

    public TTimer(Interval interval) {
        if (interval == null)
            return;

        this.interval = Utils.range((int) interval.getTime(Unit.MILLISECONDS), 1000, null);

        synchronized (list) {
            list.add(this);
        }
        thread.setPriority(3);
        thread.start();
    }

    public TTimer interval(Interval interval) {
        if (interval != null)
            this.interval = Utils.range((int) interval.getTime(Unit.MILLISECONDS), 1000, null);
        return this;
    }

    protected abstract void run() throws Exception;

    protected void onException(Throwable e) {
        Log.error(e);
    }
    private final static LinkedHashSet<TTimer> list = new LinkedHashSet<>();

    private final static TThread thread = new TThread("Timer") {

        @Override
        protected void run() throws Exception {
            while (isRunning()) {

                while (!AppContext.isInitialized())
                    Thread.sleep(100);

                long now = System.currentTimeMillis();

                int sleep = 1000;
                TTimer task = null;

                synchronized (list) {
                    for (TTimer tr : list) {

                        int diff = (int) (tr.lastRunnedTS + (long) tr.interval - now);

                        if (diff <= 0) {
                            task = tr;
                            break;
                        }

                        if (diff < sleep)
                            sleep = diff;
                    }
                }

                if (task == null) {
                    Thread.sleep(sleep);
                    continue;
                }

                try {
                    ++task.runCounter;
                    task.lastRunnedTS = now;
                    task.run();

                    long duration = System.currentTimeMillis() - now;

                    if (duration > 1000)
                        Log.warning("TIMER", "Czas wykonania metody "
                                + task.getClass().getName() + ".run() wyniósł "
                                + duration + "ms");

                } catch (Throwable e) {
                    task.onException(e);
                }
            }

        }

        @Override
        protected void onTerminate(Throwable exception) {
            for (TTimer task : list)
                try {
                    ++task.runCounter;
                    task.lastRunnedTS = System.currentTimeMillis();;
                    task.run();
                } catch (Throwable e) {
                    task.onException(e);
                }
        }
    };

}

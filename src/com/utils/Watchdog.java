package com.utils;

import com.cron.TTimer;
import com.events.Dispatcher;
import com.utils.collections.SyncList;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;

public class Watchdog {

    private final static SyncList<Watchdog> list = new SyncList<>();

    private long timeout; // ms
    private final Runnable onTimout;
    private long resetTs;
    public final long createTs = System.currentTimeMillis();
    public final Dispatcher<Runnable> onReset = new Dispatcher<>();

    private final static TTimer timer = TTimer.instance(
            new Interval(100, Unit.MILLISECONDS), (TTimer t) -> {

                long now = System.currentTimeMillis();

                for (Watchdog wd : list)
                    if (now - wd.resetTs > wd.timeout)
                        try {
                            wd.onTimout.run();
                        } finally {
                            if (now - wd.resetTs > wd.timeout)
                                wd.remove();
                        }

            });

    public Watchdog(Interval timeout, Runnable onTimout) {
        this.timeout = timeout.getTime(Unit.MILLISECONDS);
        this.onTimout = onTimout;
        resetTs = System.currentTimeMillis();
        list.add(this);
    }

    public Watchdog reset() {
        resetTs = System.currentTimeMillis();
        onReset.dispatch(this, r -> r.run());
        return this;
    }

    public Watchdog onReset(Object context, Runnable runnable) {
        onReset.listen(context, runnable);
        return this;
    }

    public void remove() {
        list.remove(this);
    }

    public long lastResetTS() {
        return resetTs;
    }

    public Interval getRemaining() {
        return new Interval(timeout - (System.currentTimeMillis() - resetTs), Unit.MILLISECONDS);
    }
}

package com.utils;

import com.cron.TTimer;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

/**
 * Klasa przetrzymmująca obiekty w formie mapy przez okreslony czas. Po upływie
 * czasu usuwane są z mapy;
 *
 * @author milosz
 */
public class Temp {

    long timeout;
    final long duration;
    final Object object;
    final Runnable onTimeout;
    final static TempObjTimeoutTimer timer = new TempObjTimeoutTimer();

    final static HashMap<Object, Temp> map = new HashMap<>();

    private Temp(Object key, Object value, Interval timeout, Runnable onTimeout) {
        duration = timeout.getTime(Unit.MILLISECONDS);
        this.timeout = System.currentTimeMillis() + duration;
        this.object = value;
        this.onTimeout = onTimeout;
        synchronized (map) {
            map.put(key, this);
        }
    }

    public static void put(Object key, Object value, Interval timeout) {
        new Temp(key, value, timeout, null);
    }

    public static void put(Object key, Object value, Interval timeout, Runnable onTimeout) {
        new Temp(key, value, timeout, onTimeout);
    }

    public static Temp remove(Object key) {
        synchronized (map) {
            return map.remove(key);
        }
    }

    public static <T> T pool(Object key, Class<T> clazz) {
        synchronized (map) {
            Temp obj = map.get(key);
            if (obj != null) {
                obj.keepAlive();
                map.remove(key);
                return (T) obj.object;
            }
        }
        return null;
    }

    public static <T> T get(Object key, Class<T> clazz) {
        synchronized (map) {
            Temp obj = map.get(key);
            if (obj != null) {
                obj.keepAlive();
                return (T) obj.object;
            }
        }
        return null;
    }

    private void keepAlive() {
        timeout = System.currentTimeMillis() + duration;
    }

}

class TempObjTimeoutTimer extends TTimer {

    public TempObjTimeoutTimer() {
        super(new Interval(1, Unit.SECONDS));
    }

    @Override
    protected void run() throws Exception {
        long now = System.currentTimeMillis();

        synchronized (Temp.map) {
            for (Entry<Object, Temp> en : new LinkedList<>(Temp.map.entrySet())) {
                Temp to = en.getValue();
                if (now > to.timeout) {
                    Temp.map.remove(en.getKey());
                    if (to.onTimeout != null)
                        to.onTimeout.run();
                }
            }

        }

    }

}

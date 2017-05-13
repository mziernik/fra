package com.thread;

import com.cron.TTimer;
import com.events.EventListeners;
import com.intf.runnable.Runnable1;
import com.lang.core.Language;
import com.mlogger.Log;
import com.servlet.requests.HttpRequest;
import com.servlet.views.connection.ViewWebSocketController;
import com.servlet.websocket.WebSocketConnection;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.webapi.core.WebApiRequest;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ThreadObject<T> {

    public final String name;
    public final EventListeners<Runnable1<T>> onSet = new EventListeners<>();
    public final static ThreadObject<ThreadDump> parentThread = new ThreadObject<>("ParentThread");
    public final static ThreadObject<Language> language = new ThreadObject<>("Language");
    public final static ThreadObject<HttpRequest> httpRequest = new ThreadObject<>("HttpRequest");
    public final static ThreadObject<WebSocketConnection> wsConn = new ThreadObject<>("WebSocketConnection");
    public final static ThreadObject<WebApiRequest> webApiReq = new ThreadObject<>("WebApiRequest");
    public final static ThreadObject<Runnable1<Log>> logsHandler = new ThreadObject<>("LogsHandler");
    public final static ThreadObject<ViewWebSocketController> viewWebSocketController = new ThreadObject<>("ViewWebSocketController");

    private final static Map<Thread, Map<ThreadObject<?>, Object>> threads = new HashMap<>();

    static {
        TTimer.instance(new Interval(10, Unit.SECONDS), (timer) -> {
            Thread[] arr = new Thread[Thread.activeCount()];
            Thread.enumerate(arr);

            Set<Thread> toRemove = new HashSet<>();
            synchronized (threads) {
                for (Thread th : threads.keySet()) {
                    boolean has = false;

                    for (Thread t : arr)
                        if (t == th) {
                            has = true;
                            break;
                        }
                    if (!has)
                        toRemove.add(th);
                }
                for (Thread th : toRemove)
                    threads.remove(th);
            }

        });
    }

    @Override
    public String toString() {
        Thread th = Thread.currentThread();
        return name + ", " + th.getId() + ", " + get(th);

    }

    public ThreadObject(String name) {
        this.name = name;
    }

    public ThreadObject<T> set(T value) {
        return set(Thread.currentThread(), value);
    }

    public ThreadObject<T> set(Thread thread, T value) {
        if (thread == null || value == null)
            return this;

        for (Runnable1 r : onSet)
            r.run(value);

        Map<ThreadObject<?>, Object> map;
        synchronized (threads) {
            map = threads.get(thread);
            if (map == null) {
                map = new HashMap<>();
                threads.put(thread, map);
            }

            if (this == parentThread) {
                ThreadDump dump = (ThreadDump) value;

                if (!map.containsKey(language))
                    map.put(language, dump.language);

            }

            map.put(this, value);
        }
        return this;
    }

    public T get() {
        return get(Thread.currentThread());
    }

    public T get(Thread thread) {
        synchronized (threads) {
            Map<ThreadObject<?>, Object> map = threads.get(thread);
            return map != null ? (T) map.get(this) : null;
        }
    }

    public T getF() {
        return getF(Thread.currentThread());
    }

    public T getF(Thread thread) {
        T result = get(thread);
        if (result == null)
            throw new RuntimeException(String.format("Thread object \"%s\" does not exists", name));
        return result;
    }

    public Map<ThreadObject<T>, Object> getAll() {
        return getAll(Thread.currentThread());
    }

    public Map<ThreadObject<T>, Object> getAll(Thread thread) {
        synchronized (threads) {
            Map<?, ?> map = threads.get(thread);
            Map<ThreadObject<T>, Object> result = new HashMap<>();
            if (map != null)
                result.putAll((Map<ThreadObject<T>, Object>) map);
            return result;
        }
    }

    public static Map<ThreadObject<?>, Object> clear() {
        return clear(Thread.currentThread());
    }

    public static Map<ThreadObject<?>, Object> clear(Thread thread) {
        synchronized (threads) {
            return threads.remove(thread);
        }
    }

    public T remove() {
        return remove(Thread.currentThread());
    }

    public T remove(Thread thread) {
        synchronized (threads) {
            Map<ThreadObject<?>, Object> map = threads.get(thread);
            return map != null ? (T) map.remove(this) : null;
        }
    }

    public T removeF() {
        return removeF(Thread.currentThread());
    }

    public T removeF(Thread thread) {
        T result = remove(thread);
        if (result == null)
            throw new RuntimeException(String.format("Thread object \"%s\" does not exists", name));
        return result;
    }
}

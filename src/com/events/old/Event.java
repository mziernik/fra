package com.events.old;

import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Properties;

public class Event<Param1, Param2> implements Iterable<IEvent<Param1, Param2>> {

    private final LinkedHashSet<IEvent<Param1, Param2>> listenners = new LinkedHashSet<>();
    private final String name;
    private final IOnEventException<Param1, Param2> onException;
    private final Date createTime = new Date();
    private final Properties properties = new Properties();
    private int executionCount = 0;
    private int errorCount = 0;

    public String getName() {
        return name;
    }

    public Object getProperty(String name) {
        return properties.get(name);
    }

    public Properties getProperties() {
        return properties;
    }

    public Event<Param1, Param2> setProperty(String namme, Object value) {
        properties.put(name, value);
        return this;
    }

    public int getExecutionCount() {
        return executionCount;
    }

    public int getErrorCount() {
        return errorCount;
    }

    public Date getCreateTime() {
        return createTime;
    }

    Event(String name, IOnEventException<Param1, Param2> onException) {
        this.name = name;
        this.onException = onException;
    }

    public void call(Param1 param1, Param2 param2) {

        for (IEvents event : Events.listenners)
            try {
                event.onEvent(this, false, param1, param2);
            } catch (Throwable e) {
                ++errorCount;
                if (onException != null && !onException.onEventException(param1, param2, e))
                    return;
            }

        for (IEvent<Param1, Param2> event : listenners)
            try {
                event.onEvent(this, param1, param2);
            } catch (Throwable e) {
                ++errorCount;
                if (onException != null && !onException.onEventException(param1, param2, e))
                    return;
            }
        ++executionCount;

    }

    public Thread callAsync(final Param1 param1, final Param2 param2, final Runnable onDone) {

        Thread thread = new Thread(new Runnable() {

            @Override
            public void run() {
                try {

                    for (IEvents event : Events.listenners)
                        try {
                            event.onEvent(Event.this, true, param1, param2);
                        } catch (Throwable e) {
                            ++errorCount;
                            if (onException != null && !onException.onEventException(param1, param2, e))
                                return;
                        }

                    for (IEvent<Param1, Param2> event : listenners)
                        try {
                            event.onEvent(Event.this, param1, param2);
                        } catch (Throwable e) {
                            ++errorCount;
                            if (onException != null && !onException.onEventException(param1, param2, e))
                                return;
                        }
                    ++executionCount;
                } finally {
                    if (onDone != null)
                        onDone.run();
                }
            }
        });
        thread.setName("Event: " + name);
        thread.start();
        return thread;
    }

    public Event<Param1, Param2> addListenner(IEvent<Param1, Param2> listenner) {
        listenners.add(listenner);
        return this;
    }

    @Override
    public Iterator<IEvent<Param1, Param2>> iterator() {
        return listenners.iterator();
    }
}

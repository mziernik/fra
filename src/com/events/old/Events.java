package com.events.old;

import java.util.LinkedHashSet;

public abstract class Events {

    private final static LinkedHashSet<Event<?, ?>> events = new LinkedHashSet<>();
    final static LinkedHashSet<IEvents> listenners = new LinkedHashSet<>();

    private Events() {

    }

    public static <Param1, Param2> Event<Param1, Param2>
            registerEvent(String name, IOnEventException<Param1, Param2> onException) {
        Event<Param1, Param2> event = new Event<>(name, onException);
        events.add(event);
        return event;
    }

    public static boolean deregisterEvent(Event<?, ?> event) {
        return events.remove(event);
    }

    public static void addListenner(IEvents listenner) {
        synchronized (listenners) {
            listenners.add(listenner);
        }
    }

    public static boolean removeListenner(IEvents listenner) {
        synchronized (listenners) {
            return listenners.remove(listenner);
        }
    }

}

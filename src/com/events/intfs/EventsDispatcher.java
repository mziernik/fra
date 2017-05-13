package com.events.intfs;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public abstract class EventsDispatcher<Event extends IEvent> implements Iterable<Event> {

    protected final List<Event> listeners = new LinkedList<>();

    public void addListener(Event listener) {
        listeners.add(listener);
    }

    public boolean removeListener(Event listener) {
        return listeners.remove(listener);
    }

    @Override
    public Iterator<Event> iterator() {
        return listeners.iterator();
    }

}

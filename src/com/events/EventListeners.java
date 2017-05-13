package com.events;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Klasa ułatwiająca obsługę listenerów. Jako [Intf] deklarujemy dowolny
 * interfejs. Może być nim jeden z poniższych.
 *
 * @param <Intf>
 */
public class EventListeners<Intf> implements Iterable<Intf> {

    private final Set<Intf> listeners = new LinkedHashSet<>();

    @Override
    public Iterator<Intf> iterator() {
        Set<Intf> list;
        synchronized (listeners) {
            list = new LinkedHashSet<>(listeners);
        }
        return list.iterator();
    }

    public Stream<Intf> stream() {
        Set<Intf> list;
        synchronized (listeners) {
            list = new LinkedHashSet<>(listeners);
        }
        return list.stream();
    }

    public void add(Intf runnable) {
        if (runnable != null)
            synchronized (listeners) {
                listeners.add(runnable);
            }
    }

    public void remove(Intf runnable) {
        synchronized (listeners) {
            listeners.remove(runnable);
        }
    }

    public boolean isEmpty() {
        return listeners.isEmpty();
    }

    public EventListeners<Intf> clear() {
        listeners.clear();
        return this;
    }

}

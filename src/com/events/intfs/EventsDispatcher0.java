package com.events.intfs;

public class EventsDispatcher0
        extends EventsDispatcher<IEvent0>
        implements IEvent0 {

    @Override
    public void call() throws Exception {
        for (IEvent0 event : listeners)
            event.call();
    }

}

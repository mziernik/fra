package com.events.intfs;

public class EventsDispatcher1<Arg1>
        extends EventsDispatcher<IEvent1<Arg1>>
        implements IEvent1<Arg1> {

    @Override
    public void call(Arg1 arg1) throws Exception {
        for (IEvent1 event : listeners)
            event.call(arg1);
    }

}

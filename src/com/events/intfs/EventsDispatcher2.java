package com.events.intfs;

public class EventsDispatcher2<Arg1, Arg2>
        extends EventsDispatcher<IEvent2<Arg1, Arg2>>
        implements IEvent2<Arg1, Arg2> {

    @Override
    public void call(Arg1 arg1, Arg2 arg2) throws Exception {
        for (IEvent2 event : listeners)
            event.call(arg1, arg2);
    }

}

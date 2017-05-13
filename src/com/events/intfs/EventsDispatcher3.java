package com.events.intfs;

public class EventsDispatcher3<Arg1, Arg2, Arg3>
        extends EventsDispatcher<IEvent3<Arg1, Arg2, Arg3>>
        implements IEvent3<Arg1, Arg2, Arg3> {

    @Override
    public void call(Arg1 arg1, Arg2 arg2, Arg3 arg3) throws Exception {
        for (IEvent3 event : listeners)
            event.call(arg1, arg2, arg3);
    }

}

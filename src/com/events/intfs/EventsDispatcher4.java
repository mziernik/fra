package com.events.intfs;

public class EventsDispatcher4<Arg1, Arg2, Arg3, Arg4>
        extends EventsDispatcher<IEvent4<Arg1, Arg2, Arg3, Arg4>>
        implements IEvent4<Arg1, Arg2, Arg3, Arg4> {

    @Override
    public void call(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4) throws Exception {
        for (IEvent4 event : listeners)
            event.call(arg1, arg2, arg3, arg4);
    }

}

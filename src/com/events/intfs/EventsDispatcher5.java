package com.events.intfs;

public class EventsDispatcher5<Arg1, Arg2, Arg3, Arg4, Arg5>
        extends EventsDispatcher<IEvent5<Arg1, Arg2, Arg3, Arg4, Arg5>>
        implements IEvent5<Arg1, Arg2, Arg3, Arg4, Arg5> {

    @Override
    public void call(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4, Arg5 arg5) throws Exception {
        for (IEvent5 event : listeners)
            event.call(arg1, arg2, arg3, arg4, arg5);
    }

}

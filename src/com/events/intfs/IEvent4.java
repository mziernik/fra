package com.events.intfs;

public interface IEvent4<Arg1, Arg2, Arg3, Arg4> extends IEvent {

    public void call(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4) throws Exception;
}

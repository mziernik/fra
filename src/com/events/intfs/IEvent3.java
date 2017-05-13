package com.events.intfs;

public interface IEvent3<Arg1, Arg2, Arg3> extends IEvent {

    public void call(Arg1 arg1, Arg2 arg2, Arg3 arg3) throws Exception;
}

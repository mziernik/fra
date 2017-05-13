package com.events.intfs;

public interface IEvent2< Arg1, Arg2> extends IEvent {

    public void call(Arg1 arg1, Arg2 arg2) throws Exception;
}

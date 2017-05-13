package com.events.old;

public interface IEvent<Param1, Param2> {

    public void onEvent(Event<Param1, Param2> event, Param1 param1, Param2 param2) throws Exception;
}

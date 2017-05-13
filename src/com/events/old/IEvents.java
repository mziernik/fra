package com.events.old;

public interface IEvents {

    public void onEvent(Event<?, ?> event, boolean isAsync, Object param1, Object param2) throws Exception;
}

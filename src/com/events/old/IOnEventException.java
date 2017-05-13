package com.events.old;

public interface IOnEventException<Param1, Param2> {

    public boolean onEventException(Param1 param1, Param2 param2, Throwable ex);
}

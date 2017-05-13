package com.events;

public interface NotifyListener<T extends Object> {

    public void onNotify(T event) throws Exception;

}

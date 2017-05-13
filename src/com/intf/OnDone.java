package com.intf;

@FunctionalInterface
public interface OnDone<T> {

    public void onDone(T obj, Throwable exception);
}

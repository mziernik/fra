package com.intf.callable;

@FunctionalInterface
public interface Callable<Return> {

    public Return run();
}

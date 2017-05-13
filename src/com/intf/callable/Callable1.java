package com.intf.callable;

@FunctionalInterface
public interface Callable1<Return, Arg> {

    public Return run(Arg arg);
}

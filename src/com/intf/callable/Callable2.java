package com.intf.callable;

@FunctionalInterface
public interface Callable2<Return, Arg1, Arg2> {

    public Return run(Arg1 arg1, Arg2 arg2);
}

package com.intf.callable;

@FunctionalInterface
public interface Callable5<Return, Arg1, Arg2, Arg3, Arg4, Arg5> {

    public Return run(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4, Arg5 arg5);
}

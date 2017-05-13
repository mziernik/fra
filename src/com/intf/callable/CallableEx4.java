package com.intf.callable;

@FunctionalInterface
public interface CallableEx4<Return, Arg1, Arg2, Arg3, Arg4> {

    public Return run(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4) throws Exception;
}

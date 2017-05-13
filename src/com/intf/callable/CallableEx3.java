package com.intf.callable;

@FunctionalInterface
public interface CallableEx3<Return, Arg1, Arg2, Arg3> {

    public Return run(Arg1 arg1, Arg2 arg2, Arg3 arg3) throws Exception;
}

package com.intf.runnable;

@FunctionalInterface
public interface RunnableEx4<Arg1, Arg2, Arg3, Arg4> {

    public void run(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4) throws Exception;
}

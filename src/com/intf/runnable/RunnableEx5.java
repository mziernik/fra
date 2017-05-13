package com.intf.runnable;

@FunctionalInterface
public interface RunnableEx5<Arg1, Arg2, Arg3, Arg4, Arg5> {

    public void run(Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4, Arg5 arg5) throws Exception;
}

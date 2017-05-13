package com.intf.runnable;

@FunctionalInterface
public interface RunnableEx3<Arg1, Arg2, Arg3> {

    public void run(Arg1 arg1, Arg2 arg2, Arg3 arg3) throws Exception;
}

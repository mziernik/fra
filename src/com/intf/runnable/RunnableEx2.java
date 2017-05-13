package com.intf.runnable;

@FunctionalInterface
public interface RunnableEx2<Arg1, Arg2> {

    public void run(Arg1 arg1, Arg2 arg2) throws Exception;
}

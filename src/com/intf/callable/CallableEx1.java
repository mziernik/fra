package com.intf.callable;

@FunctionalInterface
public interface CallableEx1<Return, Arg> {

    public Return run(Arg arg) throws Exception;
}

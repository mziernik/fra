package com.intf.callable;

@FunctionalInterface
public interface CallableEx<Return> {

    public Return run() throws Exception;

}

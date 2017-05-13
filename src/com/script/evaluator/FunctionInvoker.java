package com.script.evaluator;

@FunctionalInterface
public interface FunctionInvoker {

    public void invoke(String functionName, Object... arguments) throws Exception;

}

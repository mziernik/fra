package com.script.evaluator;

@FunctionalInterface
public interface VariableResolver {

    public Object resolveVariable(String name, Object value);
}

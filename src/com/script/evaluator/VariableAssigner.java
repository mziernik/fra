package com.script.evaluator;

import com.script.evaluator.element.EValue;

@FunctionalInterface
public interface VariableAssigner {

    public void assign(String name, EValue value);

}

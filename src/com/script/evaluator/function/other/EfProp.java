package com.script.evaluator.function.other;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfProp extends Function {

    public EfProp(Evaluator evaluator) {
        super(evaluator, "prop");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class);
        return System.getProperty(args[0].asString());
    }

}

package com.script.evaluator.function.other;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfThrow extends Function {

    public EfThrow(Evaluator evaluator) {
        super(evaluator, "throw");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class);
        throw new Error(args[0].asString());
    }

}

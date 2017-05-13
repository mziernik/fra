package com.script.evaluator.function.math;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfAbs extends Function {

    public EfAbs(Evaluator evaluator) {
        super(evaluator, "abs");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, Number.class);

        EValue number = args[0];

        if (number.isLong())
            return Math.abs(number.asLong());

        return Math.abs(number.asDouble());
    }
}

package com.script.evaluator.function.string;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfCharAt extends Function {

    public EfCharAt(Evaluator evaluator) {
        super(evaluator, "charAt");
    }

    @Override
    protected Character doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class, Number.class);
        return args[0].asString().charAt((int) args[1].asLong());
    }

}

package com.script.evaluator.function.string;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfContains extends Function {

    public EfContains(Evaluator evaluator) {
        super(evaluator, "contains");
    }

    @Override
    protected Boolean doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class, String.class);
        return args[0].asString().contains(args[1].asString());
    }

}

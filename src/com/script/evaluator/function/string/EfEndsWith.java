package com.script.evaluator.function.string;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfEndsWith extends Function {

    public EfEndsWith(Evaluator evaluator) {
        super(evaluator, "endsWith");
    }

    @Override
    protected Boolean doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class, String.class);
        return args[0].asString().endsWith(args[1].asString());
    }

}

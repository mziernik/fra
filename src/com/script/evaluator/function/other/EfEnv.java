package com.script.evaluator.function.other;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfEnv extends Function {

    public EfEnv(Evaluator evaluator) {
        super(evaluator, "env");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class);
        return System.getenv(args[0].asString());
    }

}

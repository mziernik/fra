package com.script.evaluator.function.string;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfLowerCase extends Function {

    public EfLowerCase(Evaluator evaluator) {
        super(evaluator, "lowerCase");
    }

    @Override
    protected String doEvaluate(EFunction funct, EValue[] args) {
        checkArgs(args, String.class);
        return args[0].asString().toLowerCase();
    }

}

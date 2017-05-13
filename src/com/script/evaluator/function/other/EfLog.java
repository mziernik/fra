package com.script.evaluator.function.other;

import com.script.evaluator.function.string.*;
import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfLog extends Function {

    public EfLog(Evaluator evaluator) {
        super(evaluator, "log");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) {
        for (EValue arg : args)
            com.mlogger.Log.log(arg.toString());
        return Void.TYPE;
    }

}

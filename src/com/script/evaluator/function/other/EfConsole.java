package com.script.evaluator.function.other;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;
import com.utils.console.TConsole;

public class EfConsole extends Function {

    public EfConsole(Evaluator evaluator) {
        super(evaluator, "console");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) {
        for (EValue arg : args)
            TConsole.print(arg.getValue());
        return Void.TYPE;
    }

}

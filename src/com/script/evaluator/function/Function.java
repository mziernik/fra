package com.script.evaluator.function;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.utils.Undefined;

public abstract class Function {

    public final String name;
    public final Evaluator evaluator;

    public Function(Evaluator evaluator, String name) {
        this.name = name;
        this.evaluator = evaluator;
    }

    public final EValue evaluate(EFunction funct, EValue[] arguments) {
        Object result;
        try {
            result = doEvaluate(funct, arguments);
        } catch (Throwable ex) {
            throw new EvaluationException(funct.pos, ex);
        }
        if (result == Void.TYPE)
            result = Undefined.TYPE;
        return new EValue(funct.parent, result, null);
    }

    protected void checkArgs(EValue[] args, Class<?>... expected) {

        boolean ok = true;
        for (int i = 0; i < expected.length; i++) {
            Class<?> cls = expected[i];

            if (args.length <= i || args[i] == null || args[i].isNull()
                    || !cls.isAssignableFrom(args[i].getValue().getClass())) {
                ok = false;
                break;
            }
        }
        if (!ok)
            throw new EvaluationException("Incorrect function arguments");
    }

    protected abstract Object doEvaluate(EFunction funct, EValue[] args) throws Exception;

}

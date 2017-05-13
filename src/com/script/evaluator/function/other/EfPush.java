package com.script.evaluator.function.other;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;
import java.util.List;

public class EfPush extends Function {

    public EfPush(Evaluator evaluator) {
        super(evaluator, "push");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) throws Exception {
        checkArgs(args, List.class, Object.class);

        //    evaluator.variables.get( args[0].)
        List<Object> list = (List<Object>) args[0].getValue();
        list.add(args[1].getValue());

        return list;
    }

}

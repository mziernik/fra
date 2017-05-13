package com.script.evaluator.function.other;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EBlock;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;

public class EfEval extends Function {

    public EfEval(Evaluator evaluator) {
        super(evaluator, "eval");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) throws Exception {
        checkArgs(args, String.class);
        EBlock block = new EBlock(evaluator, funct.pos);
        block.parse(null, args[0].asString());
        return block.eval().getValue();
    }

}

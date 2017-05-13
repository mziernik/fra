package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class NotOperator extends UnaryOperator {

    public NotOperator(Evaluator evaluator) {
        super(evaluator, "!", 0);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue unary) {

        if (unary.isBoolean())
            return !unary.asBoolean();

        if (unary.isLong())
            return ~unary.asLong();

        return null;
    }

}

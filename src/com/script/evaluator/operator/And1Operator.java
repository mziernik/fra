package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class And1Operator extends Operator {

    /**
     * Default constructor.
     */
    public And1Operator(Evaluator evaluator) {
        super(evaluator, "&", 2);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isBoolean() && right.isBoolean())
            return left.asBoolean() && right.asBoolean();

        if (left.isNumber() && right.isNumber())
            return left.asLong() & right.asLong();

        return null;
    }
}

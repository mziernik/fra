package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class LessThanOrEqualOperator extends Operator {

    public LessThanOrEqualOperator(Evaluator evaluator) {
        super(evaluator, "<=", 4);
    }

    @Override
    protected Boolean doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isDouble() && right.isDouble())
            return left.asDouble() <= right.asDouble();

        if (left.isString() && right.isString())
            return left.asString().compareTo(right.asString()) <= 0;

        return null;
    }
}

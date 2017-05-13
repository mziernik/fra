package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class GreaterThanOrEqualOperator extends Operator {

    public GreaterThanOrEqualOperator(Evaluator evaluator) {
        super(evaluator, ">=", 4);
    }

    public double evaluate(final double leftOperand, final double rightOperand) {
        if (leftOperand >= rightOperand)
            return 1;

        return 0;
    }

    @Override
    protected Boolean doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isDouble() && right.isDouble())
            return left.asDouble() >= right.asDouble();

        if (left.isString() && right.isString())
            return left.asString().compareTo(right.asString()) >= 0;

        return null;
    }
}

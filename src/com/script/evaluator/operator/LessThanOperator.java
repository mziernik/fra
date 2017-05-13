package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class LessThanOperator extends Operator implements NumericOperator {

    /**
     * Default constructor.
     */
    public LessThanOperator(Evaluator evaluator) {
        super(evaluator, "<", 4);
    }

    @Override
    public Object doEvaluate(EOperand operand, double val1, double val2) {
        return val1 < val2;
    }

    @Override
    public Object doEvaluate(EOperand operand, long val1, long val2) {
        return val1 < val2;
    }

    @Override
    protected Boolean doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isDouble() && right.isDouble())
            return left.asDouble() < right.asDouble();

        if (left.isString() && right.isString())
            return left.asString().compareTo(right.asString()) < 0;

        return null;
    }

}
package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public abstract class AndOperator extends Operator {

    /**
     * Default constructor.
     */
    public AndOperator(Evaluator evaluator, String operator) {
        super(evaluator, operator, 2);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isBoolean() && right.isBoolean())
            return left.asBoolean() && right.asBoolean();

        if (left.isLong() && right.isLong())
            return left.asLong() & right.asLong();

        return null;
    }

    public static class And1Operator extends AndOperator {

        public And1Operator(Evaluator evaluator) {
            super(evaluator, "&");
        }
    }

    public static class And2Operator extends AndOperator {

        public And2Operator(Evaluator evaluator) {
            super(evaluator, "&&");
        }
    }
}

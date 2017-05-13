package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import com.utils.Utils;
import com.utils.Is;

public abstract class OrOperator extends Operator {

    public OrOperator(Evaluator evaluator, String operator) {
        super(evaluator, "|", 1);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isBoolean() && right.isBoolean())
            return left.asBoolean() || right.asBoolean();

        if (left.isLong() && right.isLong())
            return left.asLong() | right.asLong();

        if ((left.isBoolean() && left.asBoolean())
                || (left.isLong() && left.asLong() != 0)
                || (left.isDouble() && left.asDouble() != 0)
                || (left.isString() && !Is.empty(left.asString())))
            return left;

        if ((right.isBoolean() && right.asBoolean())
                || (right.isLong() && right.asLong() != 0)
                || (right.isDouble() && right.asDouble() != 0)
                || (right.isString() && !Is.empty(right.asString())))
            return right;

        return false;
    }

    public static class Or1Operator extends OrOperator {

        public Or1Operator(Evaluator evaluator) {
            super(evaluator, "|");
        }
    }

    public static class Or2Operator extends OrOperator {

        public Or2Operator(Evaluator evaluator) {
            super(evaluator, "||");
        }
    }
}

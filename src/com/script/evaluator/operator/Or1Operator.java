package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class Or1Operator extends Operator {

    public Or1Operator(Evaluator evaluator) {
        super(evaluator, "|", 1);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isBoolean() && right.isBoolean())
            return left.asBoolean() || right.asBoolean();

        if (left.isLong() && right.isLong())
            return left.asLong() | right.asLong();

        return null;
    }

}

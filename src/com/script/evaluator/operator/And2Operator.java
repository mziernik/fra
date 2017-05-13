package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class And2Operator extends Operator {

    public And2Operator(Evaluator evaluator) {
        super(evaluator, "&&", 2);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        return new And1Operator(evaluator).doEvaluate(operand, left, right);
    }
}

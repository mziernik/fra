package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class Or2Operator extends Operator {

    public Or2Operator(Evaluator evaluator) {
        super(evaluator, "||", 1);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        return new Or1Operator(evaluator).doEvaluate(operand, left, right);
    }

}

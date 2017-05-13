package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;

public class XorOperator extends Operator {

    public XorOperator(Evaluator evaluator) {
        super(evaluator, "^", 3);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        if (left.isLong() && right.isLong())
            return left.asLong() ^ right.asLong();

        return null;
    }

}

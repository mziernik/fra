package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import java.util.Objects;

public class NotEqua1lOperator extends Operator {

    public NotEqua1lOperator(Evaluator evaluator) {
        super(evaluator, "!=", 3);
    }

    @Override
    protected Boolean doEvaluate(EOperand operand, EValue left, EValue right) {
        return !Objects.equals(left.getValue(), right.getValue());
    }

}

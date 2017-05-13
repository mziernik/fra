package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import java.util.Objects;

public abstract class NotEqualOperator extends Operator {

    public NotEqualOperator(Evaluator evaluator, String operator) {
        super(evaluator, operator, 3);
    }

    @Override
    protected Boolean doEvaluate(EOperand operand, EValue left, EValue right) {
        return !Objects.equals(left.getValue(), right.getValue());
    }

    public static class NotEqual1Operator extends NotEqualOperator {

        public NotEqual1Operator(Evaluator evaluator) {
            super(evaluator, "!=");
        }
    }

    public static class NotEqual2Operator extends NotEqualOperator {

        public NotEqual2Operator(Evaluator evaluator) {
            super(evaluator, "<>");
        }
    }
}

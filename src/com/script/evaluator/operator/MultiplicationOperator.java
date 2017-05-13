package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import java.math.BigDecimal;

public class MultiplicationOperator extends Operator implements NumericOperator {

    public MultiplicationOperator(Evaluator evaluator) {
        super(evaluator, "*", 6);
    }

    @Override
    public Double doEvaluate(EOperand operand, double val1, double val2) {
        return val1 * val2;
    }

    @Override
    public Long doEvaluate(EOperand operand, long val1, long val2) {
        return val1 * val2;
    }

    @Override
    protected Double doEvaluate(EOperand operand, EValue left, EValue right) {

        if (left.isDouble() && right.isDouble())
            return left.asDouble() * right.asDouble();

        return null;
    }
}

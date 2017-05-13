package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

public class SubtractionOperator extends UnaryOperator implements NumericOperator {

    public SubtractionOperator(Evaluator evaluator) {
        super(evaluator, "-", 5);
    }

    @Override
    public Double doEvaluate(EOperand operand, double val1, double val2) {
        return val1 - val2;
    }

    @Override
    public Long doEvaluate(EOperand operand, long val1, long val2) {
        return val1 - val2;
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {

        if (left.isArray()) {
            List<EValue> arr = left.asArray();
            for (EValue lv : new LinkedList<>(arr))
                if (Objects.equals(lv.getValue(), right.getValue()))
                    arr.remove(lv);
            return arr;
        }

        return null;
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue unary) {
        if (unary.isLong())
            return -unary.asLong();

        if (unary.isDouble())
            return -unary.asDouble();

        return null;
    }

}

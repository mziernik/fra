package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import java.util.LinkedList;
import java.util.List;

public class AdditionOperator extends UnaryOperator implements NumericOperator {

    public AdditionOperator(Evaluator evaluator) {
        super(evaluator, "+", 5);
    }

    @Override
    public Double doEvaluate(EOperand operand, double val1, double val2) {
        return val1 + val2;
    }

    @Override
    public Long doEvaluate(EOperand operand, long val1, long val2) {
        return val1 + val2;
    }

    @Override
    public Object doEvaluate(EOperand operand, EValue left, EValue right) {

        if (left.isNumber() && right.isString())
            return left.asString() + right.asString();

        if (left.isString() && right.isNumber())
            return left.asString() + right.asString();

        if (left.isArray() || right.isArray()) {
            List<EValue> result = new LinkedList<>();

            if (left.isArray())
                result.addAll(left.asArray());
            else
                result.add(left);

            if (right.isArray())
                result.addAll(right.asArray());
            else
                result.add(right);

            return result;
        }

        return left.asString() + right.asString();
    }

    @Override
    protected EValue doEvaluate(EOperand operand, EValue unary) {
        if (unary.isDouble())
            return unary;

        return null;
    }

}

package com.script.evaluator.operator;

import com.script.evaluator.element.EValue;
import com.script.evaluator.*;
import com.script.evaluator.element.EOperand;

public abstract class Operator {

    public final String symbol;
    public final int priority;
    public final Evaluator evaluator;

    public final EValue evaluate(EOperand operand, EValue left, EValue right) {
        Object result = null;
        if (this instanceof NumericOperator) {
            if (left.isLong() && right.isLong())
                result = ((NumericOperator) this).doEvaluate(operand, left.asLong(), right.asLong());

            if (result == null && left.isDouble() && right.isDouble())
                result = ((NumericOperator) this).doEvaluate(operand, left.asDouble(), right.asDouble());

        }

        if (result == null)
            result = doEvaluate(operand, left, right);

        if (result instanceof EValue)
            return (EValue) result;

        if (result == null)
            throw new EvaluationException("Unsupported operation ("
                    + this.getClass().getSimpleName().replace("Operator", "") + "): "
                    + left + " " + symbol + " " + right);
        return new EValue(operand.parent, result, null);
    }

    public final EValue evaluate(EOperand operand, EValue unary) {
        Object result = doEvaluate(operand, unary);
        if (result == null)
            throw new EvaluationException("Unsupported operation: "
                    + symbol + unary);

        return new EValue(operand.parent, result, null);
    }

    protected Object doEvaluate(EOperand operand, EValue unary) {
        throw new EvaluationException(this.getClass().getSimpleName());
    }

    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {
        throw new EvaluationException(this.getClass().getSimpleName());
    }

    public Operator(Evaluator evaluator, String symbol, final int priority) {
        this.symbol = symbol;
        this.priority = priority;
        this.evaluator = evaluator;
        evaluator.operators.put(symbol, this);
    }

    @Override
    public boolean equals(final Object object) {
        if (object == null)
            return false;

        if (!(object instanceof Operator))
            throw new IllegalStateException("Invalid operator object.");

        Operator operator = (Operator) object;

        if (symbol.equals(operator.symbol))
            return true;

        return false;
    }

    @Override
    public String toString() {
        return symbol;
    }
}

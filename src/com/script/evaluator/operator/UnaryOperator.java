package com.script.evaluator.operator;

import com.script.evaluator.Evaluator;

public abstract class UnaryOperator extends Operator {

    public UnaryOperator(Evaluator evaluator, String symbol, int precedence) {
        super(evaluator, symbol, precedence);
    }

}

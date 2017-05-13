package com.script.evaluator.operator;

import com.script.evaluator.element.EOperand;

public interface NumericOperator {

    Object doEvaluate(EOperand operand, double val1, double val2);

    Object doEvaluate(EOperand operand, long val1, long val2);
}

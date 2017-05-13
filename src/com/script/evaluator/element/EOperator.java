package com.script.evaluator.element;

import com.script.evaluator.Pos;
import com.script.evaluator.operator.Operator;
import com.utils.text.StrWriter;

public class EOperator extends Element {

    public final Operator operator;

    public EOperator(ECollection parent, Operator operator, Pos pos) {
        super(parent, pos);
        this.operator = operator;
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append(operator.symbol);
    }

    @Override
    public EValue eval() {
        return new EValue(parent, operator.symbol, pos);
    }

}

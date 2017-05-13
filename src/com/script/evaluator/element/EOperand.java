package com.script.evaluator.element;

import com.script.evaluator.Evaluator;
import com.script.evaluator.Pos;
import com.script.evaluator.operator.Operator;
import com.utils.text.StrWriter;

public class EOperand extends ECollection {

    public final Element left;
    public final Operator opr;
    public final Element right;

    public EOperand(ECollection parent, Element left, Operator opr, Element right, Pos pos) {
        super(parent, pos);
        this.left = left;
        this.opr = opr;
        this.right = right;
    }

    @Override
    Evaluator process() {
        if (left instanceof ECollection)
            ((ECollection) left).process();
        if (right instanceof ECollection)
            ((ECollection) right).process();
        return evaluator;
    }

    @Override
    public EValue eval() {
        if (left == null)
            return opr.evaluate(this, right.eval());

        EValue lValue = left.eval();
        EValue rValue = right.eval();
        return opr.evaluate(this, lValue, rValue);
    }

    /*
    @Override
    protected StringBuilder toString(StringBuilder sb, int level) {
        return sb.append("(")
                .append(left != null ? left.toString() + " " : "")
                .append(opr.symbol)
                .append(left != null ? " " : "")
                .append(right != null ? right.toString() : null)
                .append(")");

    }
     */
    @Override
    public void getContent(StrWriter writer) {
        if (left != null) {
            left.getContent(writer);
            writer.append(" ");
        }

        writer.append(opr.symbol);

        if (right != null) {
            writer.append(" ");
            right.getContent(writer);
        }
    }

}

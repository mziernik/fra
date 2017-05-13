package com.script.evaluator.element;

import com.script.evaluator.Evaluator;
import com.script.evaluator.Pos;
import com.utils.text.StrWriter;

// wywołania elementów rozdzielonych kropką
public class ECall extends ECollection {

    public ECall(ECollection parent, Pos pos) {
        super(parent, pos);

    }

    @Override
    Evaluator process() {

        return evaluator;
    }

    @Override
    public EValue eval() {
        return null;
    }

    @Override
    public void getContent(StrWriter writer) {
        /*
        if (left != null) {
            left.getContent(writer);
            writer.append(" ");
        }

        writer.append(opr.symbol);

        if (right != null) {
            writer.append(" ");
            right.getContent(writer);
        }*/
    }

}

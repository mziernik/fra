package com.script.evaluator.element;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Pos;
import com.utils.text.StrWriter;

// ()
public class EGroup extends ECollection {

    public EGroup(ECollection parent, Pos pos) {
        super(parent, pos);
    }

    @Override
    public EValue eval() {

        Element el = this;

        while (el instanceof EGroup) {
            if (((EGroup) el).children.size() != 1)
                throw new EvaluationException(pos, "Incorrect expression \"%s\"", el.toString());
            el = children.peekFirst();
        }

        if (el == null)
            throw new EvaluationException(pos, "Incorrect expression \"%s\"", el.toString());
        // na  tym etapie wszytko powinno być podzielone na operandy
        return el.eval();
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("(");
        super.getContent(writer);
        writer.append(")");
    }
}

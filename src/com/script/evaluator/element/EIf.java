package com.script.evaluator.element;

import com.script.evaluator.Evaluator;
import com.script.evaluator.Pos;
import com.utils.text.StrWriter;

public class EIf extends ECollection {

    public final EGroup condition;

    public EIf(ECollection parent, EGroup cond, EBlock content, Pos pos) {
        super(parent, pos);
        condition = cond;
        children.addAll(content.children);
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("if");
        condition.getContent(writer);
        writer.append("{");
        super.getContent(writer);
        writer.append("}");
    }

    @Override
    Evaluator process() {
        super.process();
        condition.process();
        return evaluator;
    }

    @Override
    public EValue evalOrDef(EValue def) {
        EValue eval = eval();
        return eval != null ? eval : def;
    }

    @Override
    public EValue eval() {
        if (!condition.eval().asBoolean())
            return null;

        EValue result = null;
        for (Element val : children)
            result = val.eval();
        return result;
    }
}

package com.script.evaluator.element;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.Pos;
import com.script.evaluator.function.Function;
import com.utils.text.StrWriter;
import java.util.ArrayList;

public class EFunction extends ECollection {

    public final String name;
    public final EGroup arguments;

    public EFunction(ECollection parent, String name, EGroup arguments, Pos pos) {
        super(parent, pos);
        this.name = name;
        this.arguments = arguments;
        arguments.parent = this;
    }

    @Override
    public Evaluator process() {
        arguments.process();
        return evaluator;
    }

    @Override
    public void getContent(StrWriter writer) {

        writer.append(name).append("(");
        arguments.getContent(writer);
        writer.append(")");
    }

    @Override
    public EValue eval() {
        Function funct = evaluator.functions.get(name);
        if (funct == null)
            throw new EvaluationException(pos, "Undefined function \"%s\"", name);

        ArrayList<EValue> vals = new ArrayList<>();

        for (Element el : arguments)
            vals.add(el.eval());

        return funct.evaluate(this, vals.toArray(new EValue[0]));
    }
}

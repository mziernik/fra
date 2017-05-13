package com.script.evaluator.element;

import com.script.evaluator.Pos;

// {} blok główny, zawartość if-a itp
public class EBlock extends ECollection {

    public EBlock(ECollection parent, Pos pos) {
        super(parent, pos);
    }

    @Override
    public EValue eval() {
        EValue result = null;
        for (Element val : children)
            result = val.evalOrDef(result);

        return result;
    }

}

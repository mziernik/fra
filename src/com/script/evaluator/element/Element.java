package com.script.evaluator.element;

import com.script.evaluator.Evaluator;
import com.script.evaluator.Pos;
import com.utils.text.StrWriter;
import com.utils.text.WritableContent;
import java.util.*;

public abstract class Element implements WritableContent {

    public final Evaluator evaluator;
    public ECollection parent;

    public final Pos pos;

    final StringBuilder sb = new StringBuilder();
    boolean isString;

    public Element(ECollection parent, Pos pos) {
        this.parent = this instanceof Evaluator ? null : Objects.requireNonNull(parent);
        this.evaluator = this instanceof Evaluator ? (Evaluator) this : parent.evaluator;
        this.pos = pos;
    }

    public abstract EValue eval();

    public EValue evalOrDef(EValue def) {
        return eval();
    }

    @Override
    public String toString() {
        StrWriter writer = new StrWriter();
        getContent(writer);
        return writer.toString();
    }

    final static char[][] BLOCKS = {{'{', '}'}, {'[', ']'}, {'(', ')'}};

}

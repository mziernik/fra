package com.script.evaluator.element;

import com.script.evaluator.Pos;
import com.utils.text.StrWriter;

public class EKeyword extends Element {

    public EKeyword(ECollection parent, Pos pos) {
        super(parent, pos);
    }

    private String value;

    public String value() {
        if (value == null)
            value = sb.toString().trim();
        return value;
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append(value);
    }

    @Override
    public EValue eval() {
        return new EValue(parent, value, pos);
    }

}

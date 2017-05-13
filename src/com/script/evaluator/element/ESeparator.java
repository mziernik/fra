package com.script.evaluator.element;

import com.script.evaluator.Pos;
import com.utils.text.StrWriter;

public class ESeparator extends Element {

    public final char character;

    public ESeparator(ECollection parent, char character, Pos pos) {
        super(parent, pos);
        this.character = character;
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append(character);
    }

    public boolean isComma() {
        return character == ',';
    }

    public boolean isNewLine() {
        return character == '\n' || character == ';';
    }

    @Override
    public EValue eval() {
        return new EValue(parent, character, pos);
    }
}

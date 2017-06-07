package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType_old;
import com.json.JElement;

import java.io.IOException;

public class CcChar extends ConfigCell<Character, Character> {

    public CcChar(CharSequence name) {
        super(DataType_old.TEXT, Character.class, name);
        min = 1;
        max = 1;
    }

    @Override
    public Character doParse(JElement json) throws IOException {
        return json.asValue().asString().charAt(0);
    }

}

package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;


public class CcEmail extends ConfigCell<String, String> {

    public CcEmail(CharSequence name) {
        super(DataType.TEXT, String.class, name);
    }

    @Override
    public String doParse(JElement json) throws Exception {
        return json.asValue().asString();
    }

}

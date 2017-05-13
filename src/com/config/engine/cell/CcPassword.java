package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;


public class CcPassword extends ConfigCell<String, String> {

    public CcPassword(CharSequence name) {
        super(DataType.TEXT, String.class, name);
    }

    @Override
    public String doParse(JElement json) throws Exception {
        return json.asValue().asString();
    }

    @Override
    protected String getDisplayValueRAW(String value, boolean publicValue) {
        if (value == null || !publicValue)
            return value;

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < value.length(); i++)
            sb.append("*");
        return sb.toString();
    }

}

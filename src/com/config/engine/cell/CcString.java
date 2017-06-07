package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType_old;
import com.json.JElement;
import com.json.JNull;
import com.json.JValue;

import com.utils.Utils;

public class CcString extends ConfigCell<String, String> {

    public CcString(CharSequence name) {
        super(DataType_old.TEXT, String.class, name);
    }

    @Override
    public String doParse(JElement json) throws Exception {
        return json.asValue().asString();
    }

    @Override
    protected JElement doSerialize(Object value) {
        return value == null ? new JNull() : new JValue(Utils.toString(value));
    }

}

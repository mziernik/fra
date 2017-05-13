package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;


public class CcInteger extends ConfigCell<Integer, Integer> {

    public CcInteger(CharSequence name) {
        super(DataType.INT, Integer.class, name);
    }

    @Override
    public Integer doParse(JElement json) throws Exception {
        return json.asValue().asNumber().intValue();
    }

}

package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType_old;
import com.json.JElement;


public class CcInteger extends ConfigCell<Integer, Integer> {

    public CcInteger(CharSequence name) {
        super(DataType_old.INT, Integer.class, name);
    }

    @Override
    public Integer doParse(JElement json) throws Exception {
        return json.asValue().asNumber().intValue();
    }

}

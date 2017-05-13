package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;


public class CcLong extends ConfigCell<Long, Long> {

    public CcLong(CharSequence name) {
        super(DataType.INT, Long.class, name);
    }

    @Override
    public Long doParse(JElement json) throws Exception {
        return json.asValue().asNumber().longValue();
    }

}

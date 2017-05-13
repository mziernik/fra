package com.config.engine.field;

import com.config.engine.cell.CcLong;
import com.config.engine.SingleConfigField;


public class CfLong extends SingleConfigField<CfLong, Long> {

    public CfLong(String key, CharSequence name, Long defaultValue) {
        super(key, name, defaultValue, new CcLong(null));
    }

}

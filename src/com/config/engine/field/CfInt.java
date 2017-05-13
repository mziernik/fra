package com.config.engine.field;

import com.config.engine.cell.CcInteger;
import com.config.engine.SingleConfigField;


public class CfInt extends SingleConfigField<CfInt, Integer> {

    public CfInt(String key, CharSequence name, Integer defaultValue) {
        super(key, name, defaultValue, new CcInteger(null));
    }

}

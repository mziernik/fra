package com.config.engine.field;

import com.config.engine.cell.select.CcBool;
import com.config.engine.SingleConfigField;
import com.config.engine.SingleConfigField;


public class CfBool extends SingleConfigField<CfBool, Boolean> {

    public CfBool(String key, CharSequence name, Boolean defaultValue) {
        super(key, name, defaultValue, new CcBool(null));
    }

}

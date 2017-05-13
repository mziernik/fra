package com.config.engine.field;

import com.config.engine.cell.CcDouble;
import com.config.engine.SingleConfigField;


public class CfDouble extends SingleConfigField<CfDouble, Double> {

    public CfDouble(String key, CharSequence name, Double defaultValue) {
        super(key, name, defaultValue, new CcDouble(null));
    }

}

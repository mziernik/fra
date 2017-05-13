package com.config.engine.field;

import com.config.engine.cell.CcChar;
import com.config.engine.SingleConfigField;


public class CfChar extends SingleConfigField<CfChar, Character> {

    public CfChar(String key, CharSequence name, Character defaultValue) {
        super(key, name, defaultValue, new CcChar(null));
    }

}

package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.SingleConfigField;


public class CfString extends SingleConfigField<CfString, String> {

    public CfString(String key, CharSequence name) {
        super(key, name, null, new CcString(null));
    }

    public CfString(String key, CharSequence name, String defaultValue) {
        super(key, name, defaultValue, new CcString(null));
    }

}

package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.SingleConfigField;
import com.config.engine.cell.CcLString;
import com.lang.core.LString;


public class CfLString extends SingleConfigField<CfLString, LString> {

    public CfLString(String key, CharSequence name) {
        super(key, name, null, new CcLString(null));
    }

    public CfLString(String key, CharSequence name, LString defaultValue) {
        super(key, name, defaultValue, new CcLString(null));
    }

}

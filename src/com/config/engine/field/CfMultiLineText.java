package com.config.engine.field;

import com.config.engine.SingleConfigField;
import com.config.engine.cell.CcStringMultiLine;


public class CfMultiLineText extends SingleConfigField<CfMultiLineText, String> {

    public CfMultiLineText(String key, CharSequence name, String defaultValue) {
        super(key, name, defaultValue, new CcStringMultiLine(null));
    }

}

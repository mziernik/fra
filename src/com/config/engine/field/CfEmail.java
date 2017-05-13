package com.config.engine.field;

import com.config.engine.cell.CcEmail;
import com.config.engine.SingleConfigField;


public class CfEmail extends SingleConfigField<CfEmail, String> {

    public CfEmail(String key, CharSequence name, String defaultValue) {
        super(key, name, defaultValue, new CcEmail(null));
    }

}

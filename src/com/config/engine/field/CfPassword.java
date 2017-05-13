package com.config.engine.field;

import com.config.engine.SingleConfigField;
import com.config.engine.cell.CcPassword;


public class CfPassword extends SingleConfigField<CfPassword, String> {

    public CfPassword(String key, CharSequence name) {
        this(key, name, null);
    }

    public CfPassword(String key, CharSequence name, String defaultValue) {
        super(key, name, defaultValue, new CcPassword(null));
    }

}

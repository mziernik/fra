package com.config.engine.field;

import com.config.engine.cell.CcDate;
import com.config.engine.SingleConfigField;

import com.utils.date.TDate;

public class CfDate extends SingleConfigField<CfDate, TDate> {

    public CfDate(String key, CharSequence name, TDate defaultValue) {
        super(key, name, defaultValue, new CcDate(null));
    }

}

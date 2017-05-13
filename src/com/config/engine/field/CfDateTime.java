package com.config.engine.field;

import com.config.engine.cell.CcDateTime;
import com.config.engine.SingleConfigField;

import com.utils.date.TDate;

public class CfDateTime extends SingleConfigField<CfDateTime, TDate> {

    public CfDateTime(String key, CharSequence name, TDate defaultValue) {
        super(key, name, defaultValue, new CcDateTime(null));
    }

}

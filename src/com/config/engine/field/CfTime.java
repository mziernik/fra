package com.config.engine.field;

import com.config.engine.cell.CcTime;
import com.config.engine.SingleConfigField;

import com.utils.date.time.Interval;

public class CfTime extends SingleConfigField<CfTime, Interval> {

    public CfTime(String key, CharSequence name, Interval defaultValue) {
        super(key, name, defaultValue, new CcTime(null));
    }

    @Override
    protected String getRowDisplayValue(Interval val, boolean publicValue) {
        return val.toStringFrmtS();
    }

}

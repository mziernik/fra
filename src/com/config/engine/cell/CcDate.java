package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType_old;
import com.json.JElement;

import com.utils.date.TDate;

public class CcDate extends ConfigCell<TDate, TDate> {

    public CcDate(CharSequence name) {
        super(DataType_old.DATE, TDate.class, name);
    }

    @Override
    public TDate doParse(JElement json) throws Exception {
        return new TDate(json.asValue().asString());
    }

    @Override
    protected String getDisplayValueRAW(TDate value, boolean publicValue) {
        return value.toString("yyyy-MM-dd");
    }

}

package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType_old;
import com.json.JElement;

import com.utils.date.TDate;

public class CcDateTime extends ConfigCell<TDate, TDate> {

    public CcDateTime(CharSequence name) {
        super(DataType_old.TIMESTAMP, TDate.class, name);
    }

    @Override
    public TDate doParse(JElement json) throws Exception {
        return new TDate(json.asValue().asString());
    }

}

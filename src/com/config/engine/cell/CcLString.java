package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType_old;
import com.json.JElement;
import com.lang.core.LStr;
import com.lang.core.LString;


public class CcLString extends ConfigCell<LString, LString> {

    public CcLString(CharSequence name) {
        super(DataType_old.TEXT, LString.class, name);
    }

    @Override
    public LString doParse(JElement json) throws Exception {
        return new LStr(json.asValue().asString());
    }

}

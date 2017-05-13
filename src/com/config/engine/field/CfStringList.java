package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.MultipleConfigFields;


public class CfStringList extends MultipleConfigFields<CfStringList, String> {

    public CfStringList(String key, CharSequence name, String... defaultValue) {
        super(key, name, defaultValue, new CcString(null));
    }

}

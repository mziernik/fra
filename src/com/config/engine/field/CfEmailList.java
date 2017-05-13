package com.config.engine.field;

import com.config.engine.cell.CcEmail;
import com.config.engine.MultipleConfigFields;


public class CfEmailList extends MultipleConfigFields<CfEmailList, String> {

    public CfEmailList(String key, CharSequence name, String... defaultValue) {
        super(key, name, defaultValue, new CcEmail(null));
    }

}

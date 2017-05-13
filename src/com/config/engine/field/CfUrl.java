package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.SingleConfigField;
import com.config.engine.cell.CcUrl;
import com.lang.core.LStr;

import com.utils.Url;

public class CfUrl extends SingleConfigField<CfUrl, Url> {

    public CfUrl(String key, CharSequence name, Url defaultValue) {
        super(key, name, defaultValue, new CcUrl(null));
        cells[0].name(new LStr("URL"));
    }

}

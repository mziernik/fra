package com.config.engine.field;

import com.config.engine.cell.CcSize;
import com.config.engine.SingleConfigField;

import com.utils.Size;

public class CfSize extends SingleConfigField<CfSize, Size> {

    public CfSize(String key, CharSequence name, Size defaultValue) {
        super(key, name, defaultValue, new CcSize(null));
    }

}

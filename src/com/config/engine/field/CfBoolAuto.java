package com.config.engine.field;

import com.config.engine.SingleConfigField;
import com.config.engine.cell.select.CcBoolAuto;
import com.intf.BoolAuto;


// 3-stanowy Boolean
public class CfBoolAuto extends SingleConfigField<CfBoolAuto, BoolAuto> {

    public CfBoolAuto(String key, CharSequence name) {
        this(key, name, BoolAuto.AUTO);
    }

    public CfBoolAuto(String key, CharSequence name, BoolAuto defaultValue) {
        super(key, name, defaultValue, new CcBoolAuto(null));
    }

}

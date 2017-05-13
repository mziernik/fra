package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.SingleConfigField;

import com.utils.collections.Pair;

public class CfStringPair extends SingleConfigField<CfStringPair, Pair<String, String>> {

    public CfStringPair(String key, CharSequence name, Pair<String, String> defaultValue,
            CharSequence firstCellName, CharSequence secondCellName) {
        super(key, name, defaultValue, new CcString(firstCellName), new CcString(secondCellName));
    }

}

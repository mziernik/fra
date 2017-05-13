package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.SingleConfigField;

import com.utils.collections.Triple;

public class CfStringTriple extends SingleConfigField<CfStringTriple, Triple<String, String, String>> {

    public CfStringTriple(String key, CharSequence name, Triple<String, String, String> defaultValue,
            CharSequence firstCellName, CharSequence secondCellName, CharSequence thirdCellName) {
        super(key, name, defaultValue,
                new CcString(firstCellName),
                new CcString(secondCellName),
                new CcString(thirdCellName)
        );
    }

}

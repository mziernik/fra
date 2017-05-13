package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.MultipleConfigFields;

import com.utils.collections.Triple;
import java.util.Collection;

public class CfStringTripleList extends MultipleConfigFields<CfStringTripleList, Triple<String, String, String>> {

    public CfStringTripleList(String key, CharSequence name,
            CharSequence firstCellName, CharSequence secondCellName, CharSequence thirdCellName) {
        super(key, name, (Collection) null,
                new CcString(firstCellName),
                new CcString(secondCellName),
                new CcString(thirdCellName)
        );
    }

}

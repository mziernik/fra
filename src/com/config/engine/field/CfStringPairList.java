package com.config.engine.field;

import com.config.engine.cell.CcString;
import com.config.engine.MultipleConfigFields;

import com.utils.collections.Pair;
import java.util.Collection;

public class CfStringPairList extends MultipleConfigFields<CfStringPairList, Pair<String, String>> {

    public CfStringPairList(String key, CharSequence name,
            CharSequence firstCellName, CharSequence secondCellName) {
        super(key, name, (Collection) null,
                new CcString(firstCellName),
                new CcString(secondCellName));
    }

}

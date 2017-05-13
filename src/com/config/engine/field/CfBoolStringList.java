package com.config.engine.field;

import com.config.engine.cell.select.CcBool;
import com.config.engine.cell.CcString;
import com.config.engine.MultipleConfigFields;

import com.utils.collections.Pair;
import java.util.LinkedList;

public class CfBoolStringList extends MultipleConfigFields<CfBoolStringList, Pair<Boolean, String>> {

    public CfBoolStringList(String key, CharSequence name, CharSequence booleanFieldName,
            CharSequence stringFieldName) {
        super(key, name, new LinkedList<>(),
                new CcBool(booleanFieldName),
                new CcString(stringFieldName));
    }
}

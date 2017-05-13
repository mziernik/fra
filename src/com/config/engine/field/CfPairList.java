package com.config.engine.field;

import com.config.engine.ConfigCell;
import com.config.engine.MultipleConfigFields;

import com.utils.collections.Pair;

public class CfPairList<First, Second>
        extends MultipleConfigFields<CfPairList<First, Second>, Pair<First, Second>> {

    public CfPairList(String key, CharSequence name, ConfigCell<First, First> firstCell,
            ConfigCell<Second, Second> secondCell) {
        super(key, name, new Pair[0], firstCell, secondCell);
    }

//static CfPairList<Boolean, String> asd = new CfPairList<>("ads", null, new CcBool().name(new LStr("asdfdsf")), new CcString());
}

//static {
//    new Cf
//}
//public class CfPairList<First, Second> extends MultipleConfigFields<CfPairList<First, Second>, Pair<First, Second>> {
//}
//<First, Second> extends CPair<First, Second> {
//public class CfEmailRecipients extends MultipleConfigFields<CfEmailRecipients, Pair<EmailType, String>> {
//    public CfEmailRecipients(String key, CharSequence name) {
//        super(key, name, new Pair[0],
//                new CcEnum<>(EmailType.class),
//                new CcString());
//        unique = true;
//
//    }

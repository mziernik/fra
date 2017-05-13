package com.config.engine.field;

import com.config.engine.cell.select.CcEnum;


public class CfEnum<E extends Enum> extends CfAbstractSelect<CfEnum<E>, E> {

    public CfEnum(String key, CharSequence name, Class<E> enumerate, E defaultValue) {
        super(key, name, defaultValue, new CcEnum(enumerate, Enum::toString, null));
    }

}

package com.config.engine.cell.select;

import com.config.engine.cell.select.CcSelect;

import com.utils.collections.Pair;

public class CcEnum<E extends Enum> extends CcSelect<E, E> {

    @FunctionalInterface
    public static interface EnumCaption<E extends Enum> {

        public String geCaption(E value);
    }

    public CcEnum(Class<E> enumerate, CharSequence name) {
        this(enumerate, Enum::toString, name);
    }

    public CcEnum(Class<E> enumerate, EnumCaption<E> captionIntf, CharSequence name) {
        super(enumerate, name);
        enumerate(map -> {
            for (E val : enumerate.getEnumConstants())
                map.put(val.name().toLowerCase(), captionIntf.geCaption(val), val);
        });
    }

}

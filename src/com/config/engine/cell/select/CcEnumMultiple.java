package com.config.engine.cell.select;



public class CcEnumMultiple<E extends Enum> extends CcSelectMultiple<E> {

    @FunctionalInterface
    public static interface EnumCaption<E extends Enum> {

        public String geCaption(E value);
    }

    public CcEnumMultiple(Class<E> enumerate, CharSequence name) {
        this(enumerate, Enum::toString, name);
    }

    public CcEnumMultiple(Class<E> enumClass, EnumCaption<E> captionIntf, CharSequence name) {
        super(enumClass, name);
        enumerate(map -> {
            for (E val : enumClass.getEnumConstants())
                map.put(val.name().toLowerCase(), captionIntf.geCaption(val), val);

        });
    }

}

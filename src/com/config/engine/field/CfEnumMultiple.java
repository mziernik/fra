package com.config.engine.field;

import com.config.engine.cell.select.CcEnumMultiple;
import com.config.engine.SingleConfigField;

import java.util.Collection;

public class CfEnumMultiple<E extends Enum> extends SingleConfigField<CfEnumMultiple<E>, Collection<E>> {

    public final CcEnumMultiple<E> enumerate;

    public CfEnumMultiple(String key, CharSequence name, Class<E> enumerate, Collection<E> defaultValue) {
        super(key, name, defaultValue, new CcEnumMultiple(enumerate, Enum::toString, null));
        this.enumerate = (CcEnumMultiple) cells[0];
    }

    public CfEnumMultiple<E> ordered(boolean ordered) {
        enumerate.ordered(ordered);
        return this;
    }

    public CfEnumMultiple<E> list(Boolean combo) {
        this.enumerate.list(combo);
        return this;
    }
}

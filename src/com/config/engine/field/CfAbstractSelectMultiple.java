package com.config.engine.field;

import com.config.engine.MultipleConfigFields;
import com.config.engine.cell.select.CcSelectMultiple;

import java.util.Collection;

public abstract class CfAbstractSelectMultiple<SELF extends CfAbstractSelectMultiple<SELF, T>, T>
        extends MultipleConfigFields<SELF, T> {

    public CfAbstractSelectMultiple(String key, CharSequence name, CcSelectMultiple<T>... cells) {
        super(key, name, (Collection) null, cells);
    }

    public CfAbstractSelectMultiple ordered(boolean ordered) {
        ((CcSelectMultiple<T>) cells[0]).ordered(ordered);
        return this;
    }

    public CfAbstractSelectMultiple list(Boolean combo) {
        ((CcSelectMultiple<T>) cells[0]).list(combo);
        return this;
    }
}

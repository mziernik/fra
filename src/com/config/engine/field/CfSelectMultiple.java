package com.config.engine.field;

import com.config.engine.MultipleConfigFields;
import com.config.engine.cell.select.CcSelectMultiple;
import com.config.engine.cell.select.SelectEntries;
import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public class CfSelectMultiple<T> extends MultipleConfigFields<CfSelectMultiple<T>, T> {

    public CfSelectMultiple(String key, CharSequence name, Class<? extends T> clazz,
            Callable<SelectEntries<T>> enumerate) {
        super(key, name, (Collection) null, new CcSelectMultiple<T>(clazz, null)
                .enumerate(enumerate));
    }

    public CfSelectMultiple(String key, CharSequence name, Class<? extends T> clazz,
            Runnable1<SelectEntries<T>> enumerate) {
        super(key, name, (Collection) null, new CcSelectMultiple<T>(clazz, null)
                .enumerate(enumerate));
    }

    public CfSelectMultiple(Callable<Map<String, T>> enumerate, Class<? extends T> clazz,
            String key, CharSequence name) {
        super(key, name, (Collection) null, new CcSelectMultiple<T>(clazz, null)
                .enumerateSimple(enumerate));
    }

    public CfSelectMultiple(Runnable1<LinkedHashMap<String, T>> enumerate, Class<? extends T> clazz, String key, CharSequence name) {
        super(key, name, (Collection) null, new CcSelectMultiple<T>(clazz, null)
                .enumerateSimple(enumerate));
    }

    public CfSelectMultiple ordered(boolean ordered) {
        ((CcSelectMultiple<T>) cells[0]).ordered(ordered);
        return this;
    }

    public CfSelectMultiple list(Boolean combo) {
        ((CcSelectMultiple<T>) cells[0]).list(combo);
        return this;
    }
}

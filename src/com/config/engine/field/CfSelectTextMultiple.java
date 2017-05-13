package com.config.engine.field;

import com.config.engine.MultipleConfigFields;
import com.config.engine.SingleConfigField;
import com.config.engine.cell.select.CcSelectMultiple;
import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public class CfSelectTextMultiple extends SingleConfigField<CfSelectTextMultiple, Collection<String>> {

    public CfSelectTextMultiple(String key, CharSequence name, Callable<Map<String, String>> enumerate) {
        super(key, name, null, new CcSelectMultiple<String>(String.class, null)
                .enumerateSimple(enumerate));
    }

    public CfSelectTextMultiple(String key, CharSequence name,
            Runnable1<LinkedHashMap<String, String>> enumerate) {
        super(key, name, null, new CcSelectMultiple<String>(String.class, null)
                .enumerateSimple(enumerate));
    }

    public CfSelectTextMultiple ordered(boolean ordered) {
        ((CcSelectMultiple<String>) cells[0]).ordered(ordered);
        return this;
    }

    public CfSelectTextMultiple list(Boolean combo) {
        ((CcSelectMultiple<String>) cells[0]).list(combo);
        return this;
    }
}

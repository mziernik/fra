package com.config.engine.field;

import com.config.engine.cell.select.CcSelectText;
import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;

import java.util.LinkedHashMap;
import java.util.Map;

public class CfSelectText extends CfAbstractSelect<CfSelectText, String> {

    public CfSelectText(String key, CharSequence name, String defaultValue, Callable<Map<String, String>> enumerate) {
        super(key, name, defaultValue, new CcSelectText(enumerate, null));
    }

    public CfSelectText(String key, CharSequence name, String defaultValue, Runnable1<LinkedHashMap<String, String>> enumerate) {
        super(key, name, defaultValue, new CcSelectText(enumerate, null));
    }

}

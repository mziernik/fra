package com.config.engine.cell.select;

import com.config.engine.ConfigException;
import com.json.JElement;

import com.utils.Utils;
import com.utils.Is;
import java.io.IOException;

public class CcBool extends CcSelect<Boolean, Boolean> {

    public CcBool(CharSequence name) {
        super(Boolean.class, name);
        enumerate(map -> {
            map.put("true", "Tak", Boolean.TRUE);
            map.put("false", "Nie", Boolean.FALSE);
        });
    }

    @Override
    public Boolean doParse(JElement json) throws IOException {
        String val = json.asString();
        Boolean bool = Utils.strBool(val, null);
        if (bool == null)
            throw new ConfigException(String.format("Incorrect boolean value: \"s\"", val));
        return bool;
    }

    @Override
    protected String getDisplayValueRAW(Boolean value, boolean publicValue) {
        return Utils.boolToStr(value);
    }

}

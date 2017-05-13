package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;
import com.json.JValue;
import com.utils.Url;

public class CcUrl extends ConfigCell<Url, Url> {

    public CcUrl(CharSequence name) {
        super(DataType.TEXT, Url.class, name);
    }

    @Override
    public Url doParse(JElement json) throws Exception {
        return new Url(json.asValue().asString());
    }

    @Override
    public JElement serialize(Url value) {
        return new JValue(value.toString());
    }

}

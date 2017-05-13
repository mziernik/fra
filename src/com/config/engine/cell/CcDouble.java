package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;


public class CcDouble extends ConfigCell<Double, Double> {

    public CcDouble(CharSequence name) {
        super(DataType.DOUBLE, Double.class, name);
    }

    @Override
    public Double doParse(JElement json) throws Exception {
        return json.asValue().asNumber().doubleValue();
    }

}

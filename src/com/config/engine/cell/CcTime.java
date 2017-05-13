package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.JElement;
import com.json.JValue;

import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.text.SimpleDateFormat;

public class CcTime extends ConfigCell<Interval, Interval> {

    public CcTime(CharSequence name) {
        super(DataType.TIME, Interval.class, name);
    }

    @Override
    public Interval doParse(JElement json) throws Exception {
        return new Interval(0)
                .setTime(new SimpleDateFormat("hh:mm:ss")
                        .parse(json.asString()));
    }

    @Override
    public JElement serialize(Interval value) {
        return new JValue(value.toStringFrmtS());
    }

}

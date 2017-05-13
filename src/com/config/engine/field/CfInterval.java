package com.config.engine.field;

import com.config.engine.cell.CcInterval;
import com.config.engine.SingleConfigField;

import com.utils.date.time.Interval;
import com.utils.date.time.Unit;

public class CfInterval extends SingleConfigField<CfInterval, Interval> {

    private final CcInterval cell;

    public CfInterval(String key, CharSequence name, Interval defaultValue) {
        super(key, name, defaultValue, new CcInterval(null, defaultValue != null
                ? defaultValue.initPrecision() : null));
        cell = (CcInterval) cells[0];
        minUnit(Unit.MILLISECONDS);
        maxUnit(Unit.DAYS);
    }

    public CfInterval defaultUnit(Unit defaultUnit) {
        cell.defaultUnit = defaultUnit;
        return this;
    }

    public Unit defaultUnit() {
        return cell.defaultUnit;
    }

    public CfInterval minUnit(Unit minUnit) {
        cell.minUnit(minUnit);
        return this;
    }

    public CfInterval maxUnit(Unit maxUnit) {
        cell.maxUnit(maxUnit);
        return this;
    }
}

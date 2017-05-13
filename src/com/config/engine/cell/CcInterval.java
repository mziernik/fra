package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.config.engine.ConfigException;
import com.json.JArray;
import com.json.JElement;

import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.date.time.UnitMode;
import java.util.LinkedHashMap;

public class CcInterval extends ConfigCell<Interval, Interval> {

    public Unit defaultUnit;
    Unit minUnit;
    Unit maxUnit;

    public CcInterval(CharSequence name) {
        this(name, null);
    }

    public CcInterval(CharSequence name, Unit defaultUnit) {
        super(DataType.INTERVAL, Interval.class, name);
        this.defaultUnit = defaultUnit;

        units(() -> {
            LinkedHashMap<String, String> map = new LinkedHashMap<>();
            for (Unit u : Unit.values()) {
                if (minUnit != null && u.ordinal() > minUnit.ordinal())
                    continue;

                if (maxUnit != null && u.ordinal() < maxUnit.ordinal())
                    continue;
                map.put(u.shortName, u.name.toString());
            }
            return map;
        });

        selectedUnit = defaultUnit != null ? defaultUnit.shortName : Unit.SECONDS.shortName;
    }

    public CcInterval minUnit(Unit minUnit) {
        this.minUnit = minUnit;
        return this;
    }

    public CcInterval maxUnit(Unit maxUnit) {
        this.maxUnit = maxUnit;
        return this;
    }

    @Override
    public Interval doParse(JElement json) throws Exception {

        if (json.isArray() && json.asArray().size() == 2) {
            JArray arr = json.asArray();

            Number val = arr.element(0).asValue().asNumber();
            String sunit = arr.element(1).asValue().asString();

            Unit unit = Unit.get(sunit);
            if (unit == null)
                throw new ConfigException("Unknown unit: " + sunit);

            return new Interval(val.intValue(), unit);
        }
        throw new UnsupportedOperationException();
    }

    public CcInterval defaultUnit(Unit defaultUnit) {
        this.defaultUnit = defaultUnit;
        return this;
    }

    @Override
    protected String getDisplayValueRAW(Interval value, boolean publicValue) {
        return value.format().unitMode(UnitMode.FULL).unitSpace(" ").toString();
    }

}

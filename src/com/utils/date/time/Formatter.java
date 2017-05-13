package com.utils.date.time;

import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;

/**
 * @author Miłosz Ziernik
 * @date 09 grudnia 2015
 * @encoding UTF-8
 */
public class Formatter {

    private Unit base;
    private Unit precision;
    private boolean includeZeroValues = false;
    private String separator = " ";
    private String unitSpace = " ";
    private UnitMode unitMode = UnitMode.SHORT;
    private boolean constValLen = false;
    private final TTime time;

    public Formatter(TTime time) {
        this.time = time;
    }

    public Formatter base(Unit base) {
        this.base = base;
        return this;
    }

    public Formatter precision(Unit precision) {
        this.precision = precision;
        return this;
    }

    public Formatter includeZeroValues(boolean includeZeroValues) {
        this.includeZeroValues = includeZeroValues;
        return this;
    }

    public Formatter constValLen(boolean constValLen) {
        this.constValLen = constValLen;
        return this;
    }

    public Formatter unitMode(UnitMode unitMode) {
        this.unitMode = Utils.coalesce(unitMode, UnitMode.NONE);
        return this;
    }

    public Formatter separator(String separator) {
        this.separator = separator;
        return this;
    }

    public Formatter unitSpace(String unitSpace) {
        this.unitSpace = unitSpace;
        return this;
    }

    @Override
    public String toString() {
        return toString(false);
    }

    public String toString(boolean masked) {

        TreeMap<Unit, Long> map = time.getParts(base, precision);

        StrWriter str = new StrWriter();

        for (Map.Entry<Unit, Long> en : map.entrySet()) {

            Long val = en.getValue();
            Unit unit = en.getKey();

            boolean mask = masked && unit.order >= Unit.HOURS.order;

            if (!mask && !includeZeroValues && val == 0)
                continue;

            if (!str.isEmpty() && mask)
                switch (unit) {
                    case MINUTES:
                    case SECONDS:
                        str.append(":");
                        break;
                    case MILLISECONDS:
                        str.append(".");
                        break;
                    default:
                        str.append(separator);
                }

            boolean end = !mask && !constValLen && unit == Unit.MILLISECONDS && precision == null;

            String sval = val.toString();

            if (end) {
                Long us = map.get(Unit.MICROSECONDS);
                if (us != null && us > 0)
                    sval = Utils.formatFloat((val * 1000 + us) / 1000d);
            }

            if (!mask && !str.isEmpty())
                str.append(separator);

            if (!end && (mask || (constValLen && unit.multiper > 10)))
                if (unit.multiper > 100)
                    str.appendFrmt("%03d", val);
                else
                    str.appendFrmt("%02d", val);
            else
                str.append(sval);

            if (!mask && unitMode != UnitMode.NONE) {
                str.append(unitSpace);

                switch (unitMode) {
                    case SHORT:
                        str.append(unit.shortName);
                        break;
                    case FULL:
                        str.append(unit.getFullUnit(val));
                        break;
                }
            }

            if (end)
                break;
        }

        if (str.isEmpty())
            str.append("0")
                    .append(unitSpace)
                    .append(precision != null
                            ? precision.shortName
                            : Unit.MILLISECONDS.shortName);

        return (time.negative() ? "-" : "") + str.toString();

    }

}

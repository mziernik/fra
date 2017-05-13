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
@Deprecated
public class DurationFormatter {

    private Unit base;
    private Unit precision;
    private boolean includeZeroValues = false;
    private String separator = " ";
    private String unitSpace = null;
    private UnitMode unitMode = UnitMode.SHORT;
    private boolean constValLen = false;
    private final Interval time;

    public DurationFormatter(Interval time) {
        this.time = time;
    }

    public DurationFormatter base(Unit base) {
        this.base = base;
        return this;
    }

    public DurationFormatter precision(Unit precision) {
        this.precision = precision;
        return this;
    }

    public DurationFormatter includeZeroValues(boolean includeZeroValues) {
        this.includeZeroValues = includeZeroValues;
        return this;
    }

    public DurationFormatter constValLen(boolean constValLen) {
        this.constValLen = constValLen;
        return this;
    }

    public DurationFormatter unitMode(UnitMode unitMode) {
        this.unitMode = Utils.coalesce(unitMode, UnitMode.NONE);
        return this;
    }

    public DurationFormatter separator(String separator) {
        this.separator = separator;
        return this;
    }

    public DurationFormatter unitSpace(String unitSpace) {
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

        int elements = 0;

        for (Map.Entry<Unit, Long> en : map.entrySet()) {

            Long val = en.getValue();
            Unit unit = en.getKey();

            boolean mask = masked && unit.order >= Unit.HOURS.order;
            boolean end = !mask && !constValLen && unit == Unit.NANOSECONDS && precision == null;

            end |= !masked && elements == 1;

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

            if (!mask && end) // zaokrąglenie
                switch (unit) {
                    case SECONDS: {
                        Long next = map.get(Unit.MILLISECONDS);
                        if (next != null && next >= 500)
                            ++val;
                        break;
                    }
                    case MILLISECONDS: {
                        Long next = map.get(Unit.MICROSECONDS);
                        if (next != null && next >= 500)
                            ++val;
                        break;
                    }
                    case MICROSECONDS: {
                        Long next = map.get(Unit.NANOSECONDS);
                        if (next != null && next >= 500)
                            ++val;
                        break;
                    }
                }

            String sval = val.toString();

            if (!mask && !str.isEmpty())
                str.append(separator);

            if (!end && (mask || (constValLen && unit.multiper > 10)))
                if (unit.multiper > 100)
                    str.appendFrmt("%03d", val);
                else
                    str.appendFrmt("%02d", val);
            else
                str.append(sval);

            ++elements;

            appendUnit(str, mask, unit, val);

            if (end)
                break;
        }

        if (str.isEmpty()) {
            str.append("0");
            appendUnit(str, masked, Utils.coalesce(precision, time.getPrecision(), Unit.MILLISECONDS), 0l);
        }

        return (time.negative() ? "-" : "") + str.toString();

    }

    private void appendUnit(StrWriter str, boolean mask, Unit unit, Long val) {

        String space = unitSpace != null ? unitSpace
                : unitMode == UnitMode.FULL ? " " : "";

        if (!mask && unitMode != UnitMode.NONE) {
            str.append(space);

            switch (unitMode) {
                case SHORT:
                    str.append(unit.shortName);
                    break;
                case FULL:
                    str.append(unit.getFullUnit(val));
                    break;
            }
        }
    }

}

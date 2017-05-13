package com.utils.date.time;

import com.utils.Utils;
import com.utils.Is;
import java.util.*;
import java.util.Map.Entry;

/**
 * @author Miłosz Ziernik
 * @date 07 grudnia 2015
 * @encoding UTF-8
 */
public class TTime extends Number implements Iterable<Entry<Unit, Long>>, Cloneable {

    private Unit defaultUnit = Unit.MILLISECONDS;
    private final TreeMap<Unit, Long> parts = new TreeMap<>();
    private boolean negative; // czy wartość jest ujemna (np w przypadku różnicy)
    private Unit initPrecision;
    private Unit displayPrecision;

    @Deprecated
    public TTime(long value, Unit unit) {
        setTime(value, unit);
    }

    public TTime(Date date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        setTime(calendar.get(Calendar.MILLISECOND)
                + calendar.get(Calendar.SECOND) * 1000
                + calendar.get(Calendar.MINUTE) * 60 * 1000
                + calendar.get(Calendar.HOUR_OF_DAY) * 60 * 60 * 1000,
                Unit.MILLISECONDS
        );
    }

    public boolean isSame(TTime other) {
        return isSame(other, null);
    }

    public boolean isSame(TTime other, Unit precision) {
        if (other == null)
            return false;

        if (precision == null)
            precision = Unit.NANOSECONDS;

        TreeMap<Unit, Long> curr = getParts(null, precision);
        TreeMap<Unit, Long> oth = other.getParts(null, precision);

        if (curr.size() != oth.size())
            return false;

        for (Unit u : curr.keySet())
            if (!Objects.equals(curr.get(u), oth.get(u)))
                return false;

        return true;
    }

    public TTime() {
        this(0, Unit.MILLISECONDS);
    }

    public TTime(int hour, int minute, int second) {
        this(second
                + minute * 60
                + hour * 60 * 60,
                Unit.SECONDS);
    }

    public TTime(int hour, int minute, int second, int millisecond) {
        this(millisecond
                + second * 1000
                + minute * 60 * 1000
                + hour * 60 * 60 * 1000,
                Unit.MILLISECONDS);
    }

    public long getTime(Unit unit) {
        long val = 0;

        long multipier = 1l;

        Utils.enumRange(unit, Unit.DAYS);

        Unit prev = unit.lower();
        if (prev != null && (double) parts.get(prev) / (double) prev.multiper >= 0.5d)
            ++val;

        for (Unit u : Utils.enumRange(unit, Unit.DAYS)) {
            prev = u.lower();
            if (u != unit && prev != null && prev.multiper > 0)
                multipier *= prev.multiper;

            val += (parts.get(u) * multipier);
        }

        if (negative)
            val *= -1;
        return val;
    }

    /**
     * Ustawia wartość czasu (nadpisuje całość)
     *
     * @param value
     * @param unit
     * @return
     */
    public TTime setTime(long value, Unit unit) {
        initPrecision = unit;
        negative = value < 0;
        if (negative)
            value *= -1;

        while (unit != null) {
            parts.put(unit, unit.multiper > 0 ? value % unit.multiper : value);
            if (unit.multiper > 0)
                value /= unit.multiper;
            unit = unit.higher();
        }

        for (Unit u : Unit.values())
            if (!parts.containsKey(u))
                parts.put(u, 0l);
        return this;
    }

    public TTime add(long value, Unit unit) {
        if (unit == null)
            return this;
        TTime other = new TTime(value, unit);
        Unit precision = getTopPrecision(other);
        setTime(getTime(precision) + other.getTime(precision), precision);
        return this;
    }

    public TTime set(long value, Unit part) {
        if (part != null)
            parts.put(part, Utils.range(value, 0l,
                    part != Unit.YEARS ? part.multiper - 1 : Integer.MAX_VALUE));
        return this;
    }

    public TTime diff(Date other) {
        return diff(new TTime(other));
    }

    /**
     * Zwraca różnicę czasu pomiędzy bieżącym a {other}
     *
     * @param other
     * @return
     */
    public TTime diff(TTime other) {
        if (other == null)
            return null;

        Unit precision = getTopPrecision(other);

        return new TTime(this.getTime(precision) - other.getTime(precision), precision);
    }

    public boolean negative() {
        return negative;
    }

    public TTime negative(boolean negative) {
        this.negative = negative;
        return this;
    }

    public Map<Unit, Long> getParts() {
        Map<Unit, Long> map = new LinkedHashMap<>();
        map.putAll(parts);
        return map;
    }

    public long getPart(Unit unit) {
        return parts.get(unit);
    }

    @Override
    public String toString() {
        return format()
                .precision(Utils.coalesce(displayPrecision))
                .toString();
    }

    public TreeMap<Unit, Long> getParts(Unit base, Unit precision) {

        if (base != null && precision != null && base.order > precision.order) {
            Unit u = base;
            base = precision;
            precision = u;
        }

        if (base == null)
            for (Unit u : Unit.values()) {
                base = u;
                if (parts.get(u) > 0)
                    break;
            }

        if (precision == null)
            for (Unit u : Utils.reverse(Unit.values()))
                if (u.order >= base.order) {
                    precision = u;
                    if (parts.get(u) > 0)
                        break;
                }

        TreeMap<Unit, Long> map = (TreeMap<Unit, Long>) parts.clone();

        long baseValue = map.get(base);
        for (Unit u : Unit.values()) {
            if (u.order >= base.order)
                break;
            baseValue += map.get(u) * u.lower().multiper;
            map.remove(u);
        }
        map.put(base, baseValue);

        Unit next = precision.lower();
        if (next != null) {
            // zaokrąglenie
            long val = getPart(next);
            // zwiększ ostatnią wartość jeśli kolejna po podzieleniu jest większa lub równa 0.5
            if ((double) val / (double) next.multiper >= 0.5d)
                map.put(precision, Utils.coalesce(map.get(precision), 0l) + 1);
        }

        {
            Unit u = precision.lower();
            while (u != null) {
                map.remove(u);
                u = u.lower();
            }
        }

        return map;
    }

    public Formatter format() {
        return new Formatter(this);
    }

    public String toStringFrmtS() {
        return format()
                .base(Unit.HOURS)
                .precision(Unit.SECONDS)
                .unitMode(UnitMode.NONE)
                .toString(true);
    }

    public String toStringFrmtMS() {
        return format()
                .base(Unit.HOURS)
                .precision(Unit.MILLISECONDS)
                .unitMode(UnitMode.NONE)
                .toString(true);
    }

    public String toStringFrmt(Unit base, Unit precision) {
        return format()
                .base(base)
                .precision(precision)
                .unitMode(UnitMode.NONE)
                .toString(true);
    }

    public TTime displayPrecision(Unit displayPrecision) {
        this.displayPrecision = displayPrecision;
        return this;
    }

    public Unit displayPrecision() {
        return displayPrecision;
    }

    @Override
    public Iterator<Entry<Unit, Long>> iterator() {
        return parts.entrySet().iterator();
    }

    @Override
    protected TTime clone() throws CloneNotSupportedException {
        return (TTime) super.clone();
    }

    @Override
    public int intValue() {
        return (int) getTime(defaultUnit);
    }

    @Override
    public long longValue() {
        return (int) getTime(defaultUnit);
    }

    @Override
    public float floatValue() {
        return getTime(defaultUnit);
    }

    @Override
    public double doubleValue() {
        return getTime(defaultUnit);
    }

    public Unit getPrecision() {
        Unit precision = Unit.DAYS;
        for (Entry<Unit, Long> en : parts.entrySet())
            if (en.getValue() != null
                    && en.getValue() > 0
                    && en.getKey().ordinal() > precision.ordinal())
                precision = en.getKey();

        if (precision == Unit.DAYS && parts.get(Unit.DAYS) == 0)
            return initPrecision;

        return precision;
    }

    private Unit getTopPrecision(TTime other) {
        if (other == null)
            return getPrecision();
        Unit curr = getPrecision();
        Unit oth = other.getPrecision();
        return curr.order > oth.order ? curr : oth;
    }
}

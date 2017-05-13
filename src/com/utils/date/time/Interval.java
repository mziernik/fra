package com.utils.date.time;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.json.JSON;
import com.utils.Utils;
import com.utils.Is;
import java.io.IOException;
import java.time.Duration;
import java.util.*;
import java.util.Map.Entry;

/**
 * @author Miłosz Ziernik
 * @date 07 grudnia 2015
 * @encoding UTF-8
 */
//ToDo : rozszerzyć klasę java.time.Duration
public class Interval implements Iterable<Entry<Unit, Long>> {

    // private Unit defaultUnit = Unit.MILLISECONDS;
    private final TreeMap<Unit, Long> parts = new TreeMap<>();
    private boolean negative; // czy wartość jest ujemna (np w przypadku różnicy)
    private Unit initPrecision = Unit.MILLISECONDS;
    private Unit displayPrecision;

    public Interval(long value, Unit unit) {
        setTime(value, unit);
    }

    /**
     * Wartość double reprezentująca upływ czasu w milisekundach
     *
     * @param value
     */
    public Interval(double value) {
        this((long) value);
    }

    public Interval(long value) {
        setTime(value, Unit.MILLISECONDS);
    }

    public Interval setTime(Date date) {
        initPrecision = Utils.coalesce(Unit.MILLISECONDS, initPrecision);
        if (date == null)
            return this;

        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        setTime(cal.get(Calendar.MILLISECOND)
                + cal.get(Calendar.SECOND) * 1000
                + cal.get(Calendar.MINUTE) * 60 * 1000
                + cal.get(Calendar.HOUR_OF_DAY) * 60 * 60 * 1000,
                Unit.MILLISECONDS);
        return this;
    }

    public Interval(Interval interval) {
        this(interval.getPart(interval.getPrecision()), interval.getPrecision());
    }

    public boolean isSame(Interval other) {
        return isSame(other, null);
    }

    public double asDouble() {
        double result = getTime(Unit.MILLISECONDS);
        result += (double) getPart(Unit.MICROSECONDS) / 1000d;
        return result;
    }

    public boolean isSame(Interval other, Unit precision) {
        if (other == null)
            return false;

        if (precision == null)
            precision = Unit.MILLISECONDS;

        TreeMap<Unit, Long> curr = getParts(null, precision);
        TreeMap<Unit, Long> oth = other.getParts(null, precision);

        if (curr.size() != oth.size())
            return false;

        for (Unit u : curr.keySet())
            if (!Objects.equals(curr.get(u), oth.get(u)))
                return false;

        return true;
    }

    public Interval() {
        this(0, Unit.MILLISECONDS);
    }

    public Interval(int hour, int minute, int second) {
        this(second
                + minute * 60
                + hour * 60 * 60,
                Unit.SECONDS);
    }

    public Interval(int hour, int minute, int second, int millisecond) {
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
    public Interval setTime(long value, Unit unit) {
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

    public Interval add(long value, Unit unit) {
        if (unit == null)
            return this;
        Interval other = new Interval(value, unit);
        Unit precision = getTopPrecision(other);
        setTime(getTime(precision) + other.getTime(precision), precision);
        return this;
    }

    public Interval setPart(long value, Unit part) {
        if (part != null)
            parts.put(part, Utils.range(value, 0l,
                    part != Unit.YEARS ? part.multiper - 1 : Integer.MAX_VALUE));
        return this;
    }

    /**
     * Zwraca różnicę czasu pomiędzy bieżącym a {other}
     *
     * @param other
     * @return
     */
    public Interval diff(Interval other) {
        if (other == null)
            return null;

        Unit precision = getTopPrecision(other);

        return new Interval(this.getTime(precision) - other.getTime(precision), precision);
    }

    public boolean negative() {
        return negative;
    }

    public Interval negative(boolean negative) {
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
        return format().toString();
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
                if (base != null && u.order >= base.order) {
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

    public DurationFormatter format() {
        return new DurationFormatter(this);
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

    public Interval displayPrecision(Unit displayPrecision) {
        this.displayPrecision = displayPrecision;
        return this;
    }

    public Unit initPrecision() {
        return initPrecision;
    }

    public Unit displayPrecision() {
        return displayPrecision;
    }

    @Override
    public Iterator<Entry<Unit, Long>> iterator() {
        return parts.entrySet().iterator();
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

    private Unit getTopPrecision(Interval other) {
        if (other == null)
            return getPrecision();
        Unit curr = getPrecision();
        Unit oth = other.getPrecision();
        return curr.order > oth.order ? curr : oth;
    }

    static {
        JSON.registerAdapter(Interval.class, new IntervalAdapter());
    }

    static class IntervalAdapter extends TypeAdapter<Interval> {

        @Override
        public void write(JsonWriter out, Interval value) throws IOException {
            out.beginArray()
                    .value(value.getTime(value.initPrecision))
                    .value(value.initPrecision.shortName)
                    .endArray();
        }

        @Override
        public Interval read(JsonReader in) throws IOException {
            throw new UnsupportedOperationException();
        }

    }
}

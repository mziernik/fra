package com.utils;

import com.json.JArray;
import java.util.Objects;

public class Size extends Number {

    private long value;
    private SizeUnit unit = SizeUnit.B;

    public static enum SizeUnit {
        B(1),
        KB(1024),
        MB(KB.multiplier * 1024),
        GB(MB.multiplier * 1024);

        public final long multiplier;

        private SizeUnit(long multiplier) {
            this.multiplier = multiplier;
        }
    }

    public Size(long value) {
        this.value = value;
    }

    public Size(long value, SizeUnit unit) {
        this.unit = Objects.requireNonNull(unit, "Size Unit");
        this.value = value * unit.multiplier;
    }

    public String toStringFrmt() {
        return getValue(unit) + " " + unit.name();
    }

    public long getValue(SizeUnit unit) {
        long val = this.value;
        if (unit != SizeUnit.B)
            val /= unit.multiplier;
        return val;
    }

    public SizeUnit unit() {
        return unit;
    }

    @Override
    public int intValue() {
        return (int) value;
    }

    @Override
    public long longValue() {
        return value;
    }

    @Override
    public float floatValue() {
        return value;
    }

    @Override
    public double doubleValue() {
        return value;
    }

    @Override
    public String toString() {
        return Long.toString(value);
    }

}

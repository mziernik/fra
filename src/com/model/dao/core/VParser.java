package com.model.dao.core;

import com.utils.Utils;
import com.utils.Is;
import com.utils.date.TDate;
import com.context.AppContext;
import com.lang.LUtil;
import com.lang.core.LString;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.utils.TCurrency;
import java.util.Date;
import java.util.LinkedHashSet;

/**
 * Zbiór metod ułatwiających parsowanie wartości oraz generowanie stosownych
 * komunikatów błędów Miłosz Ziernik 2013/11/23
 */
public class VParser {

    public static enum Option {
        EXISTS, // wartość istnieje (nawet jeśli jest null-em)
        LENIENT, // tryb pobłażliwy - opcjonalna konwersja, parsowanie i trimowanie tekstu
        DEF_ON_ERR // w przypadku wystąpienia błędu parsowania zwróć wartość domyślną
    }

    public class NullValueException extends IllegalArgumentException {

        public NullValueException() {
            super(eNullValue.toString(valueName));
        }
    }

    public class EmptyValueException extends IllegalArgumentException {

        public EmptyValueException() {
            super(eEmptyValue.toString(valueName));
        }
    }

    public class IncorrectValueException extends IllegalArgumentException {

        public IncorrectValueException() {
            super(eIncorrectValue.toString(valueName, Utils.toString(value)));
        }

        public IncorrectValueException(Throwable e) {
            super(eIncorrectValue.toString(valueName, Utils.toString(value)), e);
        }
    }

    public class IncorrectTypeException extends IllegalArgumentException {

        public IncorrectTypeException(String typeName) {
            super(eIncorrectValue.toString(value, typeName));
        }
    }

    public class MissingValueException extends IllegalArgumentException {

        public MissingValueException() {
            super(eMissingValue.toString(valueName));
        }
    }

    public LString eNullValue = LUtil.VALUE_NOT_FOUND_VPARSE;
    public LString eEmptyValue = LUtil.VALUE_CANT_BE_EMPTY_VPARSE;
    public LString eMissingValue = LUtil.VALUE_NOT_FOUND_VPARSE;
    public LString eIncorrectValue = LUtil.INVALID_VALUE_VPARSER;
    public LString eIncorrectType = LUtil.INVALID_VALUE_TYPE;

    public String dateFormat;
    public final Object value;
    public final String valueName;
    public boolean logExceptions = AppContext.devMode;
    public final boolean exists;
    public final boolean lenient;
    public final boolean defOnErr;

    public VParser(String valueName, Object value, Option... options) {
        this.value = value;
        this.valueName = valueName;

        LinkedHashSet<Option> opts = Utils.asSet(options);
        this.exists = opts.contains(Option.EXISTS);
        this.lenient = opts.contains(Option.LENIENT);
        this.defOnErr = opts.contains(Option.DEF_ON_ERR);

    }

    public String getStr() {
        return process(getStr(null));
    }

    public boolean isNull() {
        return value == null;
    }

    public String getStr(String def) {
        if (value instanceof String)
            return (String) value;
        String val = Utils.toString(value);
        return val != null ? val : def;
    }

    public boolean getBool() {
        return process(getBool(null));
    }

    public Boolean getBool(Boolean def) {
        if (value instanceof Boolean)
            return (Boolean) value;
        if (value != null && lenient)
            try {
                return Utils.coalesce(Utils.strBool(processStr(), def), def);
            } catch (Throwable e) {
                onParseException(e, "getBool", def);
            }
        return returnDefault(def);
    }

    public char getChar() {
        return process(getChar(null));
    }

    public Character getChar(Character def) {
        if (value instanceof Character)
            return (Character) value;

        if (lenient && (value instanceof Number))
            return (char) (((Number) value).intValue());

        if (value != null && lenient)
            try {
                String str = processStr();
                if (str != null && str.length() == 1)
                    return str.charAt(0);
            } catch (Throwable e) {
                onParseException(e, "getByte", def);
            }
        return returnDefault(def);
    }

    public byte getByte() {
        return process(getByte(null));
    }

    public Byte getByte(Byte def) {
        if (value instanceof Number)
            return ((Number) value).byteValue();
        if (value != null && lenient)
            try {
                return Byte.parseByte(processStr());
            } catch (Throwable e) {
                onParseException(e, "getByte", def);
            }
        return returnDefault(def);
    }

    public short getShort() {
        return process(getShort(null));
    }

    public Short getShort(Short def) {
        if (value instanceof Number)
            return ((Number) value).shortValue();
        if (value != null && lenient)
            try {
                return Short.parseShort(processStr());
            } catch (Throwable e) {
                onParseException(e, "getShort", def);
            }
        return returnDefault(def);
    }

    public int getInt() {
        return process(getInt(null));
    }

    public Integer getInt(Integer def) {
        if (value instanceof Number)
            return ((Number) value).intValue();
        if (value != null && lenient)
            try {
                return Integer.parseInt(processStr());
            } catch (Throwable e) {
                onParseException(e, "getInt", def);
            }
        return returnDefault(def);
    }

    public long getLong() {
        return process(getLong(null));
    }

    public Long getLong(Long def) {
        if (value instanceof Number)
            return ((Number) value).longValue();
        if (value != null && lenient)
            try {
                return Long.parseLong(processStr());
            } catch (Throwable e) {
                onParseException(e, "getLong", def);
            }
        return returnDefault(def);
    }

    public float getFloat() {
        return process(getFloat(null));
    }

    public Float getFloat(Float def) {
        if (value instanceof Number)
            return ((Number) value).floatValue();
        if (value != null && lenient)
            try {
                return Float.parseFloat(processStr());
            } catch (Throwable e) {
                onParseException(e, "getFloat", def);
            }
        return returnDefault(def);
    }

    public double getDouble() {
        return process(getDouble(null));
    }

    public Double getDouble(Double def) {
        if (value instanceof Number)
            return ((Number) value).doubleValue();
        if (value != null && lenient)
            try {
                return Double.parseDouble(processStr());
            } catch (Throwable e) {
                onParseException(e, "getDouble", def);
            }
        return returnDefault(def);
    }

    public TCurrency getCurrency() {
        return process(getCurrency(null));
    }

    public TCurrency getCurrency(double def) {
        if (value instanceof TCurrency)
            return (TCurrency) value;

        if (value instanceof Number)
            return new TCurrency(((Number) value).doubleValue());

        if (value != null && lenient)
            try {
                return TCurrency.parse(processStr());
            } catch (Throwable e) {
                onParseException(e, "getCurrency", def);
            }
        return returnDefault(new TCurrency(def));
    }

    public TCurrency getCurrency(TCurrency def) {
        if (value instanceof TCurrency)
            return (TCurrency) value;

        if (value instanceof Number)
            return new TCurrency(((Number) value).doubleValue());

        if (value != null && lenient)
            try {
                return TCurrency.parse(processStr());
            } catch (Throwable e) {
                onParseException(e, "getCurrency", def);
            }
        return returnDefault(def);
    }

    public TDate getDate() {
        return process(getDate(null));
    }

    public TDate getDate(TDate def) {
        if (value instanceof TDate)
            return (TDate) value;

        if (value instanceof Date)
            return new TDate((Date) value);

        if (value instanceof Number)
            return new TDate(((Number) value).longValue());

        if (value != null && lenient)
            try {
                return dateFormat != null
                        ? new TDate(processStr(), dateFormat)
                        : new TDate(processStr());
            } catch (Throwable e) {
                onParseException(e, "getDate", def);
            }
        return returnDefault(def);
    }

    /**
     * Przetworzenie wartości bez typu domyślnego
     *
     * @param <T>
     * @param val
     * @return
     */
    private <T> T process(T val) {
        if (val != null)
            return val;

        if (!exists)
            throw new MissingValueException();

        if (value == null)
            throw new NullValueException();

        throw new IncorrectTypeException(value.getClass().getSimpleName());
    }

    private <T> T returnDefault(T def) {
        if (value != null)
            throw new IncorrectTypeException(value.getClass().getSimpleName());
        return def;
    }

    private String processStr() {
        String str = Utils.toString(value);
        if (lenient && str != null) {
            StringBuilder sb = new StringBuilder();
            for (char c : str.toCharArray())
                if (c > 32 && c < 128)
                    sb.append(c);
            return sb.toString();
        }
        return str;

    }

    protected void onParseException(Throwable e, String method, Object def) {
        if (logExceptions)
            new Log(LogKind.WARNING)
                    .tag("VParser")
                    .tag(method)
                    .attribute("Value", value)
                    .attribute("Default", def)
                    .send();
    }
}

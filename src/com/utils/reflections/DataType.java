package com.utils.reflections;

import com.exceptions.ServiceException;
import com.google.gson.JsonElement;
import com.json.JArray;
import com.json.JObject;
import com.utils.Utils;
import com.utils.collections.TList;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import java.io.ByteArrayInputStream;
import java.net.URL;
import java.util.*;
import java.util.regex.Pattern;

/*
  dodać listę wyboru (enumeratę, jednostki)

DATE("date"),
    TIME("time"),
    INTERVAL("interval"),
    TIMESTAMP("timestamp");

UUID
REGEX
 */
public class DataType<T> {

    public static class DataTypeUnit {

        public final String key;
        public final String name;
        public final Long multipier;

        public DataTypeUnit(String key, String name, Long multipier) {
            this.key = key;
            this.name = name;
            this.multipier = multipier;
        }

        public DataTypeUnit(String key, String name) {
            this.key = key;
            this.name = name;
            this.multipier = null;
        }

    }

    public static interface Adapter<T> {

        T parse(Object value, Object parent) throws Exception;
    }

    public final static Map<String, DataType> ALL = new LinkedHashMap<>();

    public static enum JsonType {
        BOOLEAN, NUMBER, STRING, OBJECT, ARRAY
    }

    public final Class<?> clazz;
    public final JsonType type;
    public final String name;
    public final Adapter<T> adapter;

    public final TList<DataTypeUnit> units = new TList<>();
    public final Map<String, T> enumerate = new LinkedHashMap<>();

    public DataType(JsonType type, String name, Class<T> clazz, Adapter<T> adapter) {

        this.type = Objects.requireNonNull(type);
        this.name = Objects.requireNonNull(name);
        this.adapter = Objects.requireNonNull(adapter);
        this.clazz = Objects.requireNonNull(clazz);
        if (ALL.containsKey(name))
            throw new ServiceException("DataType " + name + " aleready exists");

        ALL.put(name, this);
    }

    public JObject getJson() {
        JObject result = new JObject();
        result.put("name", name);
        result.put("raw", type.name().toLowerCase());

        if (!units.isEmpty()) {
            JArray junits = result.arrayC("units");
            for (DataTypeUnit dtu : units)
                junits.array().addAll(dtu.key, dtu.name, dtu.multipier);
        }

        if (!enumerate.isEmpty())
            result.put("enumerate", enumerate);

        return result;
    }

    public final static DataType<Boolean> BOOLEAN = new DataType(JsonType.BOOLEAN, "boolean", Boolean.class,
            (value, parent) -> {
                if (value instanceof String)
                    return Boolean.parseBoolean((String) value);
                if (value instanceof Number)
                    return ((Number) value).intValue() != 0;
                return null;
            });

    public static class EnumDataType<E extends Enum<E>> extends DataType<E> {

        public EnumDataType(Class<E> clazz) {
            super(JsonType.STRING, "enum", clazz, (value, parent) -> {
                String name = Utils.toString(value);
                for (E e : clazz.getEnumConstants())
                    if (e.name().equalsIgnoreCase(name))
                        return e;
                return null;
            });
        }

    }

    public final static DataType<Object> ANY = new DataType(JsonType.STRING, "any", Object.class,
            (value, parent) -> value);

    public final static DataType<String> STRING = new DataType(JsonType.STRING, "string", String.class,
            (value, parent) -> Utils.toString(value));

    public final static DataType<String> KEY = new DataType(JsonType.STRING, "key", String.class, (value, parent) -> {
        return Utils.checkId(Utils.toString(value), true);
    });

    public final static DataType<String> FILE_NAME = new DataType(JsonType.STRING, "file_name", String.class, STRING.adapter);

    public final static DataType<String> PASSWORD = new DataType(JsonType.STRING, "password", String.class, STRING.adapter);

    public final static DataType<String> MEMO = new DataType(JsonType.STRING, "memo", String.class, STRING.adapter);

    public final static DataType<Pattern> REGEX = new DataType(JsonType.STRING, "regex", Pattern.class,
            (value, parent) -> Pattern.compile(Utils.toString(value))
    );

    public final static DataType<Byte> BYTE = new DataType(JsonType.NUMBER, "byte", Byte.class, (value, parent) -> {
        if (value instanceof Number)
            return ((Number) value).byteValue();
        return Byte.parseByte(Utils.toString(value));
    });

    public final static DataType<Short> SHORT = new DataType(JsonType.NUMBER, "short", Short.class, (value, parent) -> {
        if (value instanceof Number)
            return ((Number) value).shortValue();
        return Short.parseShort(Utils.toString(value));
    });

    public final static DataType<Integer> INT = new DataType(JsonType.NUMBER, "int", Integer.class, (value, parent) -> {
        if (value instanceof Number)
            return ((Number) value).intValue();
        return Integer.parseInt(Utils.toString(value));
    });

    public final static DataType<Long> LONG = new DataType(JsonType.NUMBER, "long", Long.class, (value, parent) -> {
        if (value instanceof Number)
            return ((Number) value).longValue();
        return Long.parseLong(Utils.toString(value));
    });

    public final static DataType<Double> FLOAT = new DataType(JsonType.NUMBER, "float", Double.class,
            (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).doubleValue();
                String val = Utils.toString(value).replace(",", ".");
                return Double.parseDouble(val);
            });

    public final static DataType<Long> SIZE = new DataType(JsonType.NUMBER, "size", Long.class, (value, parent) -> {
        if (value instanceof Number)
            return ((Number) value).longValue();
        return Long.parseLong(Utils.toString(value));
    });

    static {
        SIZE.units.add(new DataTypeUnit("b", "B", 0l));
        SIZE.units.add(new DataTypeUnit("kb", "KB", 1024l));
        SIZE.units.add(new DataTypeUnit("mb", "MB", 1024l * 1024l));
        SIZE.units.add(new DataTypeUnit("gb", "GB", 1024l * 1024l * 1024l));
    }

    public final static DataType<UUID> UUID = new DataType(JsonType.STRING, "uid", UUID.class, (value, parent) -> {
        if (value instanceof byte[])
            return java.util.UUID.nameUUIDFromBytes((byte[]) value);
        return java.util.UUID.fromString(Utils.toString(value));
    });

    public final static DataType<TDate> DATE = new DataType(JsonType.NUMBER, "date", TDate.class, (value, parent) -> {
        if (value instanceof Date)
            return new TDate((Date) value);
        if (value instanceof Number)
            return new TDate(((Number) value).longValue());
        return new TDate(Utils.toString(value));
    });

    public final static DataType<TDate> TIME = new DataType(JsonType.NUMBER, "time", TDate.class, DATE.adapter);

    public final static DataType<TDate> TIMESTAMP = new DataType(JsonType.NUMBER, "timestamp", TDate.class, DATE.adapter);

    public final static DataType<Interval> DURATION = new DataType(JsonType.NUMBER, "duration", Interval.class, (value, parent) -> {
        if (value instanceof Number)
            return new Interval(((Number) value).doubleValue());
        return null;
    });

    static {
        DURATION.units.add(new DataTypeUnit("ms", "milisekund", 0l));
        DURATION.units.add(new DataTypeUnit("s", "sekund", 1000l));
        DURATION.units.add(new DataTypeUnit("m", "minut", 1000l * 60l));
        DURATION.units.add(new DataTypeUnit("h", "godzin", 1000l * 60l * 60l));
        DURATION.units.add(new DataTypeUnit("d", "dni", 1000l * 60l * 60l * 24l));
    }

    public final static DataType<TList> LIST = new DataType(JsonType.ARRAY, "list", Collection.class, (value, parent) -> {
        return new TList<>();
    });

    public final static DataType<HashMap<?, ?>> MAP = new DataType(JsonType.OBJECT, "integer", Map.class, (value, parent) -> {
        return new HashMap();
    });

    public final static DataType<JsonElement> JSON = new DataType(JsonType.STRING, "json", JsonElement.class, (value, parent) -> {
        if (value instanceof byte[])
            return com.json.JSON.parse(new ByteArrayInputStream((byte[]) value));
        return com.json.JSON.parse(Utils.toString(value));
    });

    public final static DataType<com.xml.XML> XML = new DataType(JsonType.STRING, "xml", com.xml.XML.class, (value, parent) -> {
        if (value instanceof byte[])
            return new com.xml.XML(new ByteArrayInputStream((byte[]) value));
        return new com.xml.XML(Utils.toString(value));
    });

    public final static DataType<com.utils.CSV> CSV = new DataType(JsonType.STRING, "csv", com.utils.CSV.class, (value, parent) -> {
        return null;
    });

    public final static DataType<byte[]> HEX = new DataType(JsonType.STRING, "hex", byte[].class, (value, parent) -> {
        return com.utils.hashes.Hex.toBytes(Utils.toString(value));
    });

    public final static DataType<byte[]> BASE64 = new DataType(JsonType.STRING, "base64", byte[].class, ((value, parent) -> {
        return com.utils.hashes.Base64.decode(Utils.toString(value));
    }));

    public final static DataType<java.net.URI> URI = new DataType(JsonType.STRING, "uri", java.net.URI.class, (value, parent) -> {
        if (value instanceof URL)
            return ((URL) value).toURI();
        return new java.net.URI(Utils.toString(value));
    });

    public final static DataType<String> HTML = new DataType(JsonType.STRING, "html", String.class, STRING.adapter);

    public static DataType of(Class<?> clazz) {
        DataType result = Utils.findFirst(ALL.values(), dt -> dt.clazz == clazz);
        if (result == null)
            throw new ServiceException("Unknown data type class: " + clazz.getSimpleName());
        return result;
    }

    public static List<DataType> find(Class<?> clazz) {
        return Utils.find(ALL.values(), dt -> dt.clazz == clazz);
    }
}

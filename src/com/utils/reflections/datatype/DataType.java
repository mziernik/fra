package com.utils.reflections.datatype;

import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.google.gson.JsonElement;
import com.intf.callable.CallableEx1;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;
import com.json.JValue;
import com.utils.Utils;
import com.utils.collections.TList;
import com.utils.date.TDate;
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

        default T process(Object value, Object parent) throws Exception {
            return value != null ? parse(value, parent) : null;
        }

        default T process(Object value) throws Exception {
            return value != null ? parse(value, null) : null;
        }
    }

    public final static Map<String, DataType> ALL = new LinkedHashMap<>();

    public static enum JsonType {
        ANY, BOOLEAN, NUMBER, STRING, OBJECT, ARRAY
    }

    public final Class<?> clazz;
    public final JsonType type;
    public final String name;
    public final CharSequence description;
    private final Adapter<T> adapter;
    private final CallableEx1<Object, T> serializer;

    public final TList<DataTypeUnit> units = new TList<>();

    private DataType(JsonType type, String name, Class<T> clazz, Adapter<T> adapter) {
        this(false, type, name, null, clazz, adapter, null);
    }

    private DataType(JsonType type, String name, CharSequence description, Class<T> clazz, Adapter<T> adapter) {
        this(false, type, name, description, clazz, adapter, null);
    }

    private DataType(JsonType type, String name, CharSequence description, Class<T> clazz,
            Adapter<T> adapter, CallableEx1<Object, T> serializer) {
        this(false, type, name, description, clazz, adapter, serializer);
    }

    private DataType(JsonType type, String name, Class<T> clazz,
            Adapter<T> adapter, CallableEx1<Object, T> serializer) {
        this(false, type, name, null, clazz, adapter, serializer);
    }

    public DataType(boolean dynamic, JsonType type, String name,
            CharSequence description, Class<T> clazz,
            Adapter<T> adapter, CallableEx1<Object, T> serializer) {

        if (adapter == null && this instanceof Adapter)
            adapter = (Adapter<T>) this;

        this.serializer = serializer;
        this.type = Objects.requireNonNull(type);
        this.name = Objects.requireNonNull(name);
        this.adapter = Objects.requireNonNull(adapter);
        this.clazz = Objects.requireNonNull(clazz);
        this.description = description;

        if (!dynamic) {
            if (ALL.containsKey(name))
                throw new ServiceException("DataType " + name + " aleready exists");

            ALL.put(name, this);
        }
    }

    public Object serialize(T value) {
        try {
            return value == null ? null : serializer != null ? serializer.run(value) : value;
        } catch (RuntimeException | Error e) {
            throw e;
        } catch (Exception e) {
            throw new ThrowableException(e);
        }
    }

    public ArrayDataType<T> asArray() {
        return new ArrayDataType<>(this);
    }

    @Override
    public String toString() {
        return name + ": " + clazz.getSimpleName();
    }

    public T parse(Object object) {
        if (object == null)
            return null;

        if (clazz.isAssignableFrom(object.getClass()))
            return (T) object;

        try {
            T result = adapter.parse(object, null);

            if (result == null)
                throw new ServiceException("Nieprawidłowa wartość " + Utils.escape(object)
                        + " dla typu " + name);
            return result;
        } catch (RuntimeException | Error e) {
            throw e;
        } catch (Throwable ex) {
            throw new ServiceException(ex)
                    .details("DataType", this.name);
        }
    }

    public JObject getJson() {
        JObject result = new JObject();
        result.put("name", name);
        result.put("raw", type.name().toLowerCase());

        if (description != null)
            result.put("desc", description.toString());

        if (!units.isEmpty()) {
            JArray junits = result.arrayC("units");
            for (DataTypeUnit dtu : units)
                junits.array().addAll(dtu.key, dtu.name, dtu.multipier);
        }

        return result;
    }

    public final static DataType<Object> ANY = new DataType<>(JsonType.ANY,
            "any", "Dowolny typ", Object.class,
            (value, parent) -> value);

    public final static DataType<Boolean> BOOLEAN = new DataType<>(JsonType.BOOLEAN,
            "boolean", Boolean.class,
            (value, parent) -> {
                if (value instanceof String)
                    return Boolean.parseBoolean((String) value);
                if (value instanceof Number)
                    return ((Number) value).intValue() != 0;
                return null;
            });

    public final static DataType<String> STRING = new DataType<>(JsonType.STRING,
            "string", String.class,
            (value, parent) -> Utils.toString(value));

    public final static DataType<String> KEY = new DataType<String>(JsonType.STRING,
            "key", String.class, (value, parent) -> {
                String id = Utils.toString(value);
                Utils.checkId(id, true);
                return id;
            });

    public final static DataType<String> EMAIL = new DataType<String>(JsonType.STRING,
            "email", String.class, (value, parent) -> Utils.toString(value));

    public final static DataType<String> FILE_NAME = new DataType<>(JsonType.STRING,
            "file_name", String.class, STRING.adapter);

    public final static DataType<String> PASSWORD = new DataType<>(JsonType.STRING,
            "password", String.class, STRING.adapter);

    public final static DataType<String> MEMO = new DataType<>(JsonType.STRING,
            "memo", "Tekst wielo liniowy", String.class, STRING.adapter);

    public final static DataType<Pattern> REGEX = new DataType<>(JsonType.STRING,
            "regex", "Wyrażenie regularne", Pattern.class,
            (value, parent) -> Pattern.compile(Utils.toString(value))
    );

    public final static DataType<Byte> BYTE = new DataType<>(JsonType.NUMBER,
            "byte", "Wartość -128...127", Byte.class, (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).byteValue();
                return Byte.parseByte(Utils.toString(value));
            });

    public final static DataType<Short> SHORT = new DataType<>(JsonType.NUMBER,
            "short", "Wartość -32768...32767", Short.class, (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).shortValue();
                return Short.parseShort(Utils.toString(value));
            });

    public final static DataType<Integer> INT = new DataType<>(JsonType.NUMBER,
            "int", "Wartość 0x80000000...0x7fffffff", Integer.class, (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).intValue();
                return Integer.parseInt(Utils.toString(value));
            });

    public final static DataType<Long> LONG = new DataType<>(JsonType.NUMBER,
            "long", Long.class, (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).longValue();
                return Long.parseLong(Utils.toString(value));
            });

    public final static DataType<Float> FLOAT = new DataType<>(JsonType.NUMBER,
            "float", Float.class,
            (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).floatValue();
                String val = Utils.toString(value).replace(",", ".");
                return Float.parseFloat(val);
            });

    public final static DataType<Double> DOUBLE = new DataType<>(JsonType.NUMBER,
            "double", Double.class,
            (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).doubleValue();
                String val = Utils.toString(value).replace(",", ".");
                return Double.parseDouble(val);
            });

    public final static DataType<Float> PERCENT = new DataType<>(JsonType.NUMBER,
            "percent", "Wartość procentowa", Float.class,
            (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).floatValue();
                String val = Utils.toString(value).replace(",", ".");
                return Float.parseFloat(val);
            });

    public final static DataType<Long> SIZE = new DataType<>(JsonType.NUMBER,
            "size", "Zapis wielkości binarnej (bajty)", Long.class, (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).longValue();
                return Long.parseLong(Utils.toString(value));
            });

    public final static DataType<UUID> UUID = new DataType<>(JsonType.STRING,
            "uid", "Unikalny identyfikator (GUID)", UUID.class, (value, parent) -> {
                if (value instanceof byte[])
                    return java.util.UUID.nameUUIDFromBytes((byte[]) value);
                return java.util.UUID.fromString(Utils.toString(value));
            });

    public final static DataType<TDate> DATE = new DataType<>(JsonType.NUMBER,
            "date", "Data (dzień, miesiąc, rok", TDate.class, (value, parent) -> {
                if (value instanceof Date)
                    return new TDate((Date) value);
                if (value instanceof Number)
                    return new TDate(((Number) value).longValue());
                return new TDate(Utils.toString(value));
            });

    public final static DataType<TDate> TIME = new DataType<>(JsonType.NUMBER,
            "time", "Godzina (godzina, minuta, sekundy, milisekundy", TDate.class, DATE.adapter);

    public final static DataType<TDate> TIMESTAMP = new DataType<>(JsonType.NUMBER,
            "timestamp", "Znacznik czasu - data i godzina", TDate.class, DATE.adapter, date -> date.getTime());

    public final static DataType<Long> DURATION = new DataType<>(JsonType.NUMBER,
            "duration", "Upływ czasu", Long.class, (value, parent) -> {
                if (value instanceof Number)
                    return ((Number) value).longValue();
                return null;
            });

    public final static DataType<TList> LIST = new DataType<>(JsonType.ARRAY,
            "list", TList.class, (value, parent) -> {
                return new TList<>();
            });

    public final static DataType<HashMap> MAP = new DataType<>(JsonType.OBJECT,
            "map", HashMap.class, (value, parent) -> {
                return new HashMap<>();
            });

    public final static EnumDataType<DataType> DATA_TYPE = new EnumDataType<>(
            DataType.class, DataType.ALL.values(),
            dt -> dt.name,
            dt -> dt.description != null ? dt.description.toString() : dt.name);

    public final static DataType<JElement> JSON = new DataType<>(JsonType.STRING,
            "json", JElement.class, (value, parent) -> {
                if (value instanceof byte[])
                    return com.json.JSON.parse(new ByteArrayInputStream((byte[]) value));
                return com.json.JSON.parse(Utils.toString(value));
            });

    public final static DataType<com.xml.XML> XML = new DataType<>(JsonType.STRING,
            "xml", com.xml.XML.class, (value, parent) -> {
                if (value instanceof byte[])
                    return new com.xml.XML(new ByteArrayInputStream((byte[]) value));
                return new com.xml.XML(Utils.toString(value));
            });

    public final static DataType<com.utils.CSV> CSV = new DataType<>(JsonType.STRING,
            "csv", com.utils.CSV.class, (value, parent) -> {
                return null;
            });

    public final static DataType<byte[]> HEX = new DataType<>(JsonType.STRING,
            "hex", "Wartość binarna zakodowana w postaci szesnastkowej", byte[].class, (value, parent) -> {
                return com.utils.hashes.Hex.toBytes(Utils.toString(value));
            });

    public final static DataType<byte[]> BASE64 = new DataType<>(JsonType.STRING,
            "base64", "Wartość binarna zakodowana w postaci Base64", byte[].class, ((value, parent) -> {
                return com.utils.hashes.Base64.decode(Utils.toString(value));
            }));

    public final static DataType<java.net.URI> URI = new DataType<>(JsonType.STRING,
            "uri", "Adres URI/URL", java.net.URI.class, (value, parent) -> {
                if (value instanceof URL)
                    return ((URL) value).toURI();
                return new java.net.URI(Utils.toString(value));
            });

    public final static DataType<String> HTML = new DataType<>(JsonType.STRING,
            "html", String.class, STRING.adapter);

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

package com.json;

import com.utils.Utils;
import com.utils.Is;
import com.google.gson.*;
import com.google.gson.adapter.*;
import com.google.gson.stream.JsonReader;
import com.lang.core.LString;
import com.resources.core.ResData;
import com.utils.Str;
import com.utils.TObject;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import java.io.*;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.*;

public abstract class JSON {

    private JSON() {
    }

    private final static Map<Class<?>, TypeAdapter<?>> adapters = new HashMap<>();

    public static <T, A extends TypeAdapter<T>> A registerAdapter(Class<T> cls, A adapter) {
        synchronized (adapters) {
            adapters.put(cls, adapter);
        }
        return adapter;
    }

    public static JElement serialize(Object object) {
        return serialize(object, new GsonBuilder());
    }

    static void registerAdapters(GsonBuilder builder) {
        builder.registerTypeHierarchyAdapter(LString.class, new LStringAdapter());
        builder.registerTypeAdapter(TObject.class, new TObjectAdapter());
        builder.registerTypeAdapter(TDate.class, new TDateAdapter());
        builder.registerTypeAdapter(Interval.class, new TTimeAdapter());

        synchronized (adapters) {
            for (Map.Entry<Class<?>, TypeAdapter<?>> en : adapters.entrySet())
                builder.registerTypeAdapter(en.getKey(), en.getValue());
        }

        //eliminacja błędu, gdy dany obiekt ma pole o identycznej nazwie jak obiekt nadrzędny
        builder.addDeserializationExclusionStrategy(new SuperclassExclusionStrategy());
        builder.addSerializationExclusionStrategy(new SuperclassExclusionStrategy());
    }

    public static JElement serialize(Object object, GsonBuilder builder) {
        registerAdapters(builder);

        if (object instanceof JElement)
            return ((JElement) object).clone();

        if ((object instanceof Boolean)
                || (object instanceof Number)
                || (object instanceof String))
            return new JValue(object);

        if (!(object instanceof Interval))
            if (object instanceof Iterable) {
                JArray arr = new JArray();
                for (Object o : (Iterable) object)
                    arr.add(o);

                return arr;
            }

        if (builder == null)
            builder = new GsonBuilder();

        final Gson gson = builder.create();
        JsonElement el = gson.toJsonTree(object);
        return convert(el);
    }

    public static JElement parse(String in) {
        return in != null ? parse(new BufferedReader(new StringReader(in))) : null;
    }

    public static JElement parse(File file) throws IOException {
        if (file == null)
            return null;

        try (BufferedInputStream in = new BufferedInputStream(new FileInputStream(file))) {
            return parse(in);
        }
    }

    public static JElement parse(URL url) throws IOException {
        if (url == null)
            return null;

        try (BufferedInputStream in = new BufferedInputStream(url.openStream())) {
            return parse(in);
        }
    }

    public static JElement parse(ResData url) throws IOException {
        if (url == null)
            return null;

        try (BufferedInputStream in = new BufferedInputStream(url.getInputStream())) {
            return parse(in);
        }
    }

    public static JElement parse(InputStream in) {
        in = Str.decode(in, Utils.UTF8);
        return parse(new BufferedReader(
                new InputStreamReader(
                        new BufferedInputStream(in), Utils.UTF8)));
    }

    /**
     * przykład:
     *
     * ({ nazwa: "wartość" });
     *
     */
    static JCollection parseJavaScript(String json) {
        if (json == null)
            return null;

        StringWriter writer = new StringWriter();

        char prev = 0;
        boolean comment = false;
        int blocks = 0; // licznik obiektów lub tablic
        char start = 0;
        char end = 0;

        for (char c : json.toCharArray())
            try {
                if (prev == '/' && c == '*') {
                    comment = true;
                    continue;
                }

                if (comment && prev == '*' && c == '/') {
                    comment = false;
                    continue;
                }

                if (comment)
                    continue;

                if (start == 0 && c == '{') {
                    start = '{';
                    end = '}';
                }

                if (start == 0 && c == '[') {
                    start = '[';
                    end = ']';
                }

                if (c == start)
                    ++blocks;

                if (c == end)
                    --blocks;

                if (c == end && blocks == 0) {
                    writer.append(c);
                    break;
                }

                if (blocks > 0)
                    writer.append(c);

            } finally {
                prev = c;
            }

        if (start == '{' && end == '}')
            return JObject.parse((writer.toString()));

        if (start == '[' && end == ']')
            return JArray.parse((writer.toString()));

        return null;
    }

    @SuppressWarnings("unchecked")
    public static JElement parse(Reader reader) {
        JsonReader jreader = new JsonReader(reader);
        jreader.setLenient(true);// mniejsze restrykcje dotyczace skladni

        JsonElement je = new JsonParser().parse(jreader);
        if (je == null)
            return null;

        if (je.isJsonNull())
            return new JValue(null);

        return convert(je);
    }

    private static Object getPrimitive(JsonPrimitive val) {
        if (val.isBoolean())
            return val.getAsBoolean();
        else if (val.isNumber())
            return val.getAsNumber();
        else if (val.isString())
            return val.getAsString();
        return null;
    }

    public static JElement convert(JsonElement source) {

        if (source == null)
            return null;

        if (source instanceof JsonNull)
            return new JNull();

        if (source instanceof JsonPrimitive)
            return new JValue(getPrimitive(source.getAsJsonPrimitive()));

        if (source instanceof JsonObject) {

            JObject obj = new JObject();

            for (Map.Entry<String, JsonElement> ee : source.getAsJsonObject().entrySet())
                obj.addElement(ee.getKey(),
                        convert(ee.getValue()), false);

            return obj;
        }

        if (source instanceof JsonArray) {
            JArray arr = new JArray();
            for (JsonElement el : source.getAsJsonArray())
                arr.addElement(null, convert(el), false);
            return arr;
        }

        return null;
    }
}

class SuperclassExclusionStrategy implements ExclusionStrategy {

    public boolean shouldSkipClass(Class<?> arg0) {
        return false;
    }

    public boolean shouldSkipField(FieldAttributes fieldAttributes) {
        String fieldName = fieldAttributes.getName();
        Class<?> theClass = fieldAttributes.getDeclaringClass();

        return isFieldInSuperclass(theClass, fieldName);
    }

    private boolean isFieldInSuperclass(Class<?> subclass, String fieldName) {
        Class<?> superclass = subclass.getSuperclass();
        Field field;

        while (superclass != null) {
            field = getField(superclass, fieldName);

            if (field != null)
                return true;

            superclass = superclass.getSuperclass();
        }

        return false;
    }

    private Field getField(Class<?> theClass, String fieldName) {
        try {
            return theClass.getDeclaredField(fieldName);
        } catch (Exception e) {
            return null;
        }
    }
}

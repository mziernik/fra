package com.utils.reflections.datatype;

import com.intf.callable.Callable1;
import com.intf.callable.CallableEx1;
import com.json.JObject;
import com.utils.Utils;
import com.utils.reflections.datatype.DataType.Adapter;
import com.utils.reflections.datatype.DataType.JsonType;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

public class EnumDataType<T> extends DataType<T> implements Adapter<T> {

    private final Map<String, T> map1 = new HashMap<>();
    private final Map<T, String> map2 = new HashMap<>();
    public final Map<String, String> enumerate = new LinkedHashMap<>();

    public static <E extends Enum<E>> EnumDataType<E> ofEnum(Class<E> clazz) {
        return new EnumDataType<>(clazz, Arrays.asList(clazz.getEnumConstants()),
                e -> e.name().toLowerCase(), E::name, (E item) -> item.name().toLowerCase());
    }

    public static <E extends Enum<E>> EnumDataType<E> ofEnum(Class<E> clazz,
            Callable1<String, E> keyProvider, Callable1<String, E> nameProvider) {
        return new EnumDataType<>(clazz, Arrays.asList(clazz.getEnumConstants()),
                keyProvider, nameProvider, (E item) -> item.name().toLowerCase());
    }

    public static <T> EnumDataType<T> ofIterable(Class<T> clazz, Iterable<T> values,
            Callable1<String, T> keyProvider,
            Callable1<String, T> nameProvider) {
        return new EnumDataType<>(clazz, values, keyProvider, nameProvider);
    }

    public static <T> EnumDataType<T> ofArray(T[] values) {
        return new EnumDataType<>((Class<T>) values.getClass().getComponentType(),
                Arrays.asList(values),
                e -> Utils.toString(e),
                e -> Utils.toString(e));
    }

    public EnumDataType(Class<T> clazz, Iterable<T> values,
            Callable1<String, T> keyProvider,
            Callable1<String, T> nameProvider) {
        this(clazz, values, keyProvider, nameProvider, null);
    }

    public EnumDataType(Class<T> clazz,
            Iterable<T> values,
            Callable1<String, T> keyProvider,
            Callable1<String, T> nameProvider,
            CallableEx1<Object, T> serializer) {
        super(true, JsonType.STRING, "enum", "Enumerata " + clazz.getSimpleName(),
                clazz, null, serializer);

        for (T e : values) {
            String key = keyProvider.run(e);
            map1.put(key, e);
            map2.put(e, key);
            enumerate.put(key, nameProvider.run(e));
        }
    }

    public boolean isEmbeddedEnum() {
        return this == ICON || this == DATA_TYPE;
    }

    @Override
    public T parse(Object value, Object parent) throws Exception {
        String key = Utils.toString(value);
        return map1.get(key);
    }

    @Override
    public Object serialize(T value) {
        return map2.get(value);
    }

    @Override
    public JObject getJson() {
        JObject json = super.getJson();
        if (!isEmbeddedEnum())
            json.put("enumerate", enumerate);
        return json;
    }

}

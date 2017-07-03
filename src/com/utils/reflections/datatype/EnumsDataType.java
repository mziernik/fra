package com.utils.reflections.datatype;

import com.intf.callable.Callable1;
import com.intf.callable.CallableEx1;
import com.json.JObject;
import com.utils.Utils;
import com.utils.reflections.datatype.DataType.Adapter;
import com.utils.reflections.datatype.DataType.JsonType;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Zbiór wartości
 *
 * @author milosz
 * @param <T>
 */
public class EnumsDataType<T> extends DataType<T[]> implements Adapter<T> {

    private final Map<String, T> map1 = new HashMap<>();
    private final Map<T, String> map2 = new HashMap<>();
    public final Map<String, String> enumerate = new LinkedHashMap<>();

    public static <E extends Enum<E>> EnumsDataType<E> ofEnum(Class<E> clazz) {
        return new EnumsDataType<>(clazz, Arrays.asList(clazz.getEnumConstants()),
                E::name, E::name, (E item) -> item.name().toLowerCase());
    }

    public static <E extends Enum<E>> EnumsDataType<E> ofEnum(Class<E> clazz,
            Callable1<String, E> keyProvider, Callable1<String, E> nameProvider) {
        return new EnumsDataType<>(clazz, Arrays.asList(clazz.getEnumConstants()),
                keyProvider, nameProvider, (E item) -> item.name().toLowerCase());
    }

    public static <T> EnumsDataType<T> ofIterable(Class<T> clazz, Iterable<T> values,
            Callable1<String, T> keyProvider,
            Callable1<String, T> nameProvider) {
        return new EnumsDataType<>(clazz, values, keyProvider, nameProvider);
    }

    public static <T> EnumsDataType<T> ofArray(T[] values) {
        return new EnumsDataType<>((Class<T>) values.getClass().getComponentType(),
                Arrays.asList(values),
                e -> Utils.toString(e),
                e -> Utils.toString(e));
    }

    public EnumsDataType(Class<T> clazz, Iterable<T> values,
            Callable1<String, T> keyProvider,
            Callable1<String, T> nameProvider) {
        this(clazz, values, keyProvider, nameProvider, null);
    }

    public EnumsDataType(Class<T> clazz, Iterable<T> values,
            Callable1<String, T> keyProvider,
            Callable1<String, T> nameProvider, CallableEx1<Object, T> serializer) {
        super(true, JsonType.ARRAY, "enums", "Enumerata " + clazz.getSimpleName(),
                (Class<T[]>) Array.newInstance(clazz, 0).getClass(), null, null);

        for (T e : values) {
            String key = keyProvider.run(e);
            map1.put(key, e);
            map2.put(e, key);
            enumerate.put(key, nameProvider.run(e));
        }
    }

    @Override
    public T parse(Object value, Object parent) throws Exception {
        String key = Utils.toString(value);
        return map1.get(key);
    }

    @Override
    public Object serialize(T[] value) {
        return map2.get(value);
    }

    @Override
    public JObject getJson() {
        JObject json = super.getJson();
        json.put("enumerate", enumerate);
        json.put("multipleEnum", true);
        return json;
    }
    
    
   

}

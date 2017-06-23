package com.utils.reflections.datatype;

import com.intf.callable.Callable1;
import com.utils.Utils;
import com.utils.reflections.datatype.DataType.Adapter;
import com.utils.reflections.datatype.DataType.JsonType;
import java.util.HashMap;
import java.util.Map;

public class EnumDataType<E extends Enum<E>> extends DataType<E> implements Adapter<E> {

    public EnumDataType(Class<E> clazz) {
        this(clazz, E::name, E::name);
    }

    private final Map<String, E> map1 = new HashMap<>();
    private final Map<E, String> map2 = new HashMap<>();

    @Override
    public E parse(Object value, Object parent) throws Exception {
        String key = Utils.toString(value);
        return map1.get(key);
    }

    @Override
    public Object serialize(E value) {
        return map2.get(value);
    }

    public EnumDataType(Class<E> clazz, Callable1<String, E> keyProvider,
            Callable1<String, E> nameProvider) {
        super(true, JsonType.OBJECT, "enum", clazz, null, (E item) -> item.name().toLowerCase());

        for (E e : clazz.getEnumConstants()) {
            String key = keyProvider.run(e);
            map1.put(key, e);
            map2.put(e, key);
            enumerate.put(key, nameProvider.run(e));
        }
    }

}

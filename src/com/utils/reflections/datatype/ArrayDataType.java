package com.utils.reflections.datatype;

import com.json.JArray;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType.JsonType;
import java.lang.reflect.Array;

public class ArrayDataType<T> extends DataType<T[]> {

    public final DataType<T> component;

    public ArrayDataType(DataType<T> component) {
        super(true, JsonType.ARRAY, component.name + "[]", "Tablica " + component.name,
                (Class<T[]>) Array.newInstance(component.clazz, 0).getClass(), (value, parent) -> {

            if (!(value instanceof Iterable))
                return null;

            TList<T> result = new TList<>();

            for (Object val : (Iterable) value)
                result.add((T) component.parse(val));

            return (T[]) result.toArray((Class<T>) component.clazz);
        }, null);
        this.component = component;
    }

    @Override
    public Object serialize(T[] value) {
        JArray arr = new JArray();
        for (T t : value)
            arr.add(component.serialize(t));
        return arr;
    }

}

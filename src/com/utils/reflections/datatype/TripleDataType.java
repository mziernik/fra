package com.utils.reflections.datatype;

import com.json.JArray;
import com.utils.collections.Triple;
import com.utils.reflections.datatype.DataType.JsonType;

public class TripleDataType<F, S, T> extends DataType<Triple<F, S, T>> {

    public final DataType<F> firstType;
    public final DataType<S> secondType;
    public final DataType<T> thirdType;

    public TripleDataType(DataType<F> firstType, DataType<S> secondType, DataType<T> thirdType) {
        super(true, JsonType.ARRAY, "(" + firstType.name + ", " + secondType.name
                + ", " + thirdType.name + ")",
                "Mapa wartości", (Class) Triple.class, (value, parent) -> {

                    //  if (value instanceof Map)
                    //    return new Pair<>((Map) value);
                    return null;
                }, t -> new JArray().addAll(t.first, t.second, t.third));

        this.firstType = firstType;
        this.secondType = secondType;
        this.thirdType = thirdType;
    }
}

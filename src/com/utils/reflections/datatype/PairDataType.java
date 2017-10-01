package com.utils.reflections.datatype;

import com.json.JArray;
import com.utils.collections.Pair;
import com.utils.reflections.datatype.DataType.JsonType;

public class PairDataType<F, S> extends DataType<Pair<F, S>> {

    public final DataType<F> firstType;
    public final DataType<S> secondType;

    public PairDataType(DataType<F> firstType, DataType<S> secondType) {
        super(true, JsonType.ARRAY, "(" + firstType.name + ", " + secondType.name + ")",
                "Mapa wartości",
                (Class) Pair.class, (value, parent) -> {

                    //  if (value instanceof Map)
                    //    return new Pair<>((Map) value);
                    return null;
                }, t -> new JArray().addAll(t.first, t.second));

        this.firstType = firstType;
        this.secondType = secondType;
    }

}

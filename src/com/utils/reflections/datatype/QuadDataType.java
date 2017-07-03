package com.utils.reflections.datatype;

import com.json.JArray;
import com.utils.collections.Quad;
import com.utils.reflections.datatype.DataType.JsonType;

public class QuadDataType<F, S, T, G> extends DataType<Quad<F, S, T, G>> {

    public final DataType<F> firstType;
    public final DataType<S> secondType;
    public final DataType<T> thirdType;
    public final DataType<G> fourthType;

    public QuadDataType(DataType<F> firstType, DataType<S> secondType, DataType<T> thirdType, DataType<G> fourthType) {
        super(true, JsonType.ARRAY, "(" + firstType.name + ", " + secondType.name
                + ", " + thirdType.name + ", " + fourthType.name + ")", "Poczwórna wartość",
                (Class) Quad.class, (value, parent) -> {

                    //  if (value instanceof Map)
                    //    return new Pair<>((Map) value);
                    return null;
              }, t -> new JArray().addAll(t.first, t.second, t.third, t.fourth));

        this.firstType = firstType;
        this.secondType = secondType;
        this.thirdType = thirdType;
        this.fourthType = fourthType;
    }
}

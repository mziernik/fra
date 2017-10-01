package com.utils.reflections.datatype;

import com.utils.reflections.datatype.DataType.JsonType;
import java.util.LinkedHashMap;
import java.util.Map;

public class MapDataType<K, V> extends DataType<LinkedHashMap<K, V>> {

    public final DataType<V> type;

    public MapDataType(DataType<V> type) {
        super(true, JsonType.OBJECT, "{" + type.name + "}",
                "Mapa wartości", (Class) LinkedHashMap.class, (value, parent) -> {

                    if (value instanceof Map)
                        return new LinkedHashMap<>((Map) value);

                    return null;
                }, null);
        this.type = type;
    }
}

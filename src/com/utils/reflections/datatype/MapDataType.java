package com.utils.reflections.datatype;

import com.utils.reflections.datatype.DataType.JsonType;
import java.util.LinkedHashMap;
import java.util.Map;

public class MapDataType<K, V> extends DataType<LinkedHashMap<K, V>> {

    public final DataType<K> keyType;
    public final DataType<V> valueType;

    public MapDataType(DataType<K> keyType, DataType<V> valueType) {
        super(true, JsonType.OBJECT, "{" + keyType.name + ", " + valueType.name + "}", null, (value, parent) -> {

            if (value instanceof Map)
                return new LinkedHashMap<>((Map) value);

            return null;
        }, null);
        this.keyType = keyType;
        this.valueType = valueType;
    }
}

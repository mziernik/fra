package com.utils.reflections.datatype;

import com.utils.Utils;
import com.utils.reflections.datatype.DataType.JsonType;

public class EnumDataType<E extends Enum<E>> extends DataType<E> {

    public EnumDataType(Class<E> clazz) {
        super(true, JsonType.STRING, "enum", clazz, (value, parent) -> {
            String name = Utils.toString(value);
            for (E e : clazz.getEnumConstants())
                if (e.name().equalsIgnoreCase(name))
                    return e;
            return null;
        });
    }
}

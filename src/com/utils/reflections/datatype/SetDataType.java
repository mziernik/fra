package com.utils.reflections.datatype;

import com.utils.Utils;
import com.utils.reflections.datatype.DataType.Adapter;
import com.utils.reflections.datatype.DataType.JsonType;
import java.util.*;

/**
 * Zbiór, podobny do enumeraty
 *
 * @author miloszz
 * @param <E>
 */
public class SetDataType extends DataType<String> implements Adapter<String> {

    public final Set<String> VALUES = new LinkedHashSet<>();

    @Override
    public String parse(Object value, Object parent) throws Exception {
        String key = Utils.toString(value);
        return VALUES.contains(key) ? key : null;
    }

    public SetDataType(String... values) {
        super(true, JsonType.STRING, "string", "Zbiór wartości unikalnych", String.class, null, null);
        VALUES.addAll(Arrays.asList(values));
        for (String s : values)
            super.values.add(s);
    }

}

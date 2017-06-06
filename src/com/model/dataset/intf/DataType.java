package com.model.dataset.intf;

import com.exceptions.ServiceException;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.utils.reflections.Reflections;
import com.utils.reflections.TClass;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.UUID;

/*
  dodać listę wyboru (enumeratę, jednostki)

DATE("date"),
    TIME("time"),
    INTERVAL("interval"),
    TIMESTAMP("timestamp");

UUID
REGEX
*/
public enum DataType {
    STRING,
    INT,
    LENGTH, // rozmiar danych w bajtach (SIZE)
    DOUBLE,
    BOOLEAN,
    DATE,
    ENUM,
    OBJECT, // map
    ARRAY,
    ; // list

    public static DataType of(Class<?> clazz) {
        if (clazz == Integer.class || clazz == Long.class)
            return DataType.INT;

        if (clazz == String.class || clazz == UUID.class)
            return DataType.STRING;

        if (clazz == Boolean.class)
            return DataType.BOOLEAN;

        if (clazz == Date.class || clazz == TDate.class)
            return DataType.DATE;

        TClass cls = new TClass(clazz);

        if (clazz.isArray() || clazz == Strings.class || cls.instanceOf(Collection.class))
            return DataType.ARRAY;

        if (cls.instanceOf(Map.class))
            return DataType.OBJECT;

        throw new ServiceException("Unknown data type class: " + clazz.getSimpleName());
    }
}

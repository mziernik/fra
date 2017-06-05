package com.model.dataset.intf;

import java.util.Date;

public enum DataType {
    STRING,
    INT,
    LENGTH, // rozmiar danych w bajtach
    DOUBLE,
    BOOLEAN,
    DATE,
    ENUM,
    OBJECT,
    ARRAY;

    public static DataType of(Class<?> clazz) {
        if (clazz == Integer.class || clazz == Long.class)
            return DataType.INT;

        if (clazz == String.class)
            return DataType.STRING;

        if (clazz == Boolean.class)
            return DataType.BOOLEAN;

        if (clazz == Date.class)
            return DataType.DATE;

        return null;
    }
}

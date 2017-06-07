package com.config.engine;

@Deprecated // użyć com.utils.reflections.DataType
public enum DataType_old {
    BOOL("bool"),
    INT("int"),
    DOUBLE("double"),
    TEXT("text"),
    SIZE("size"), // ilość danych w bajtach
    ENUM("enum"),
    DATE("date"),
    TIME("time"),
    INTERVAL("interval"),
    TIMESTAMP("timestamp");

    public final String key;

    private DataType_old(String key) {
        this.key = key;
    }

}

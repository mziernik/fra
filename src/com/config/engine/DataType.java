package com.config.engine;

@Deprecated
public enum DataType {
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

    private DataType(String key) {
        this.key = key;
    }

}

package com.model.repository;

public enum CRUDE {

    CREATE("create", "Tworzenie"),
    READ("read", "Odczyt"),
    UPDATE("update", "Modyfikacja"),
    DELETE("delete", "Usunięcie"),
    EXECUTE("execute", "Wykonanie");

    public final CharSequence title;
    public final String name;

    CRUDE(String name, CharSequence title) {
        this.name = name;
        this.title = title;
    }

    public static CRUDE get(String name) {
        if (name == null)
            return null;
        return CRUDE.valueOf(name.trim().toUpperCase());
    }
}

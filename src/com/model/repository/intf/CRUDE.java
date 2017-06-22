package com.model.repository.intf;

import com.utils.collections.Flags;

public enum CRUDE {

    CREATE("create", "Tworzenie"),
    READ("read", "Odczyt"),
    UPDATE("update", "Modyfikacja"),
    DELETE("delete", "Usunięcie"),
    EXECUTE("execute", "Wykonanie");

    public final CharSequence title;
    public final String name;

    public final static CRUDE[] CRUD = {CREATE, READ, UPDATE, DELETE};

    CRUDE(String name, CharSequence title) {
        this.name = name;
        this.title = title;
    }

    public static CRUDE get(String name) {
        if (name == null)
            return null;
        return CRUDE.valueOf(name.trim().toUpperCase());
    }

    public static Flags<CRUDE> flags(CRUDE... values) {
        return new Flags<>(cr -> cr.name().charAt(0), values);
    }
}

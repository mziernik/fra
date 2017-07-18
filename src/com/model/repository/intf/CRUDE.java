package com.model.repository.intf;

import com.utils.collections.Flags;

public enum CRUDE {

    CREATE('C', "create", "Tworzenie"),
    READ('R', "read", "Odczyt"),
    UPDATE('U', "update", "Modyfikacja"),
    DELETE('D', "delete", "Usunięcie"),
    EXECUTE('E', "execute", "Wykonanie");

    public final CharSequence title;
    public final String name;
    public final char shortcut;

    public final static CRUDE[] CRUD = {CREATE, READ, UPDATE, DELETE};
    public final static CRUDE[] CRU = {CREATE, READ, UPDATE};

    CRUDE(char shortcut, String name, CharSequence title) {
        this.name = name;
        this.title = title;
        this.shortcut = shortcut;
    }

    public static CRUDE get(String name) {
        if (name == null)
            return null;
        return CRUDE.valueOf(name.trim().toUpperCase());
    }

    public static Flags<CRUDE> flags(CRUDE... values) {
        return new Flags<>(cr -> cr.shortcut, cr -> cr.title.toString(), values);
    }
}

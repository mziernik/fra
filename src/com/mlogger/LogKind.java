package com.mlogger;

import com.exceptions.ServiceException;

public enum LogKind {

    REQUEST("Żądanie", 'R'),
    TRACE("Szczegółowy", 'T'),
    DEBUG("Debug", 'D'),
    EVENT("Zdarzenie", 'E'),
    LOG("Log", 'L'),
    INFO("Informacja", 'I'),
    WARNING("Ostrzeżenie", 'W'),
    ERROR("Błąd", 'Z'),
    EXCEPTION("Wyjątek", 'X'),
    QUERY("Zapytanie", 'Q');
    public final String name;

    public final char key;

    public static LogKind get(char key) {
        key = Character.toUpperCase(key);
        for (LogKind lk : values())
            if (lk.key == key)
                return lk;
        throw new ServiceException("Unknown log kind: " + key);
    }

    private LogKind(final String name, char key) {
        this.name = name;
        this.key = key;
    }

    @Override
    public String toString() {
        return name;
    }

}

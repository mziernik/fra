package com.mlogger;

public enum LogAttr {

    LOGGER(1, "lgr", "logger", "Logger", 100),
    CLAZZ(2, "cls", "class", "Klasa", 300),
    METHOD(3, "mth", "method", "Metoda", 200),
    LEVEL(4, "lvl", "level", "Poziom", 10),
    LEVEL_NAME(5, "lvn", "level_name", "Nawa poziomu", 30),
    UID(6, "uid", "uid", "UID", 40),
    KIND(7, "knd", "kind", "Rodzaj", true, false, 30),
    DATE(8, "dte", "date", "Data", true, false, 26),
    KEYS(9, "key", "keys", "Klucze", false, true, 30),
    MODE(10, "mde", "mode", "Tryb", false, false, 1),
    COUNTER(11, "cnt", "counter", "Licznik", true, false, 20),
    //----------
    SOURCE(20, "src", "source", "Źródło", true, false, 100),
    ADDRESS(21, "adr", "address", "Adres", false, true, 100),
    DEVICE(22, "dev", "device", "Urządzenie", 100),
    HOST_NAME(23, "hst", "hostname", "Nazwa hosta", 100),
    OS(24, "os", "os", "System operacyjny", 200),
    USER_AGENT(25, "ua", "user_agent", "UserAgent", 300),
    USER_NAME(26, "usr", "user", "Użytkownik", 100),
    TAG(27, "tag", "tag", "Tag", false, true, 100),
    VALUE(28, "val", "value", "Wartość", 30000),
    COMMENT(30, "com", "comment", "Komentarz", 200),
    //-------
    INSTANCE(40, "ist", "instance", "Instancja", 50),
    SESSION(41, "ses", "session", "Sesja", 50),
    REQUEST(42, "req", "request", "Żądanie", 50),
    VERSION(43, "ver", "version", "Wersja", 20),
    //--------
    PROCESS(50, "prc", "process_id", "ID procesu", 10),
    THREAD(51, "thr", "thread_id", "ID wątku", 10),
    THREAD_NAME(52, "thn", "thread_name", "Nazwa wątku", 100),
    THREAD_PRIORITY(53, "thp", "thread_priority", "Priorytet wątku", 20),
    //----
    COLOR(60, "fcl", "color", "Kolor czcionki", 20),
    BACKGROUND(61, "bcl", "background", "Kolor tła", 20),
    URL(62, "url", "url", "URL", false, true, 200),
    //-----------
    CALL_STACK(70, "cst", "call_stack", "Stos metod", false, true, 300),
    ERROR_STACK(71, "est", "error_stack", "Stos błędów", false, true, 300),
    ATTRIBUTE(72, "atr", "attributes", "Atrybuty", false, true, 1000),
    DATA(73, "dta", "data", "Dane", false, true, 30000),
    SOURCE_CODE(74, "scd", "source_code", "Kod źródłowy", false, false, 10000),
    //---------
    FLAGS(80, "flg", "flags", "Flagi", false, true, 1000),
    //-----------
    FIELD_KEY(101, "key", "key", "Klucz pola", 100),
    PROGRESS(102, "prg", "progress", "Postęp", 100),
    GROUP(103, "grp", "group", "Grupa", 1000);

    public final int id;
    public final String title;
    public final String name;
    public final String key;
    public final boolean required;
    public final boolean multiple;

    public int maxValueLength = 10000; // maksymalna dlugosc wartosci
    public static int maxStackTraceItemsCount = 100;

    private LogAttr(int id, String key, String name, String title, boolean requred, boolean multiple, int maxLength) {
        this.name = name;
        this.title = title;
        this.key = key;
        this.required = requred;
        this.maxValueLength = maxLength;
        this.id = id;
        this.multiple = multiple;
    }

    private LogAttr(int id, String key, String name, String title, int maxLength) {
        this.name = name;
        this.title = title;
        this.key = key;
        this.required = false;
        this.multiple = false;
        this.maxValueLength = maxLength;
        this.id = id;
    }

    public static LogAttr getByKey(String key) {
        for (LogAttr a : LogAttr.values())
            if (a.key.equals(key))
                return a;
        return null;
    }

    public static LogAttr get(int id) {
        for (LogAttr a : LogAttr.values())
            if (a.id == id)
                return a;
        return null;
    }

    public boolean isIn(Iterable<String> items) {
        if (items != null)
            for (String s : items)
                if (key.equalsIgnoreCase(s) || name.equalsIgnoreCase(s))
                    return true;
        return false;
    }
}

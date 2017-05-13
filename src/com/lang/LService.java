package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Service")
public enum LService implements LString {

    ACTIVE("Aktywny", "Active"),
    ACTIVE_2("Aktywne", "Active"),
    ADDRESS("Adres", "Address"),
    DATA("Data", "Date"),
    ERRORS("Błędy", "Errors"),
    EVENT("Zdarzenie", "Event"),
    EVENTS("Zdarzenia", "Events"),
    ID("ID", "ID"),
    LACK_OF_NEEDED_DATA("Brak wymaganych danych", "Lack of needed data"),
    LIMITED_RESULT("Liczba rezultatów została ograniczona do %1 rekordów", "Number of results has been limited to %1 records"),
    NAME("Nazwa", "Name"),
    NO_EVENT_FILTER_IMPLEMENTATION("Brak implementacji filtrowania zdarzeń", "Lack of implementation of filtering events"),
    NO_EVENT_HANDLER_IMPLEMENTATION("Brak implementacji obsługi zdarzeń", "Lack of implementation of the event handler"),
    NO_MAPPING_KEY_FOR_FILTER("Brak mapowania klucza dla filtru \"%1\"", "No mapping key for the filter \"%1\""),
    QUEUE("Kolejka", "Queue"),
    SCHEDULER("Harmonogram", "Scheduler"),
    SOURCE("Źródło", "Source"),
    TAGS("Tagi", "Tagi"),
    TASK_SCHEDULER("Harmonogram zadań", "Task Scheduler"),
    TYPE("Typ", "Type"),
    USER("Użytkownik", "User"),;

    // <editor-fold defaultstate="collapsed">
    private LService(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}

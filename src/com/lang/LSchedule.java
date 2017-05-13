package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Schedule")
public enum LSchedule implements LString {
    SCHEDULE_ACTIVE("Harmonogram aktywny", "Schedule active"),
    INTERVAL("Interwał", "Interval"),
    REF_TIME("Czas ref.", "Reference time"),
    ON_OFF_GROUP("Wł / Wył grupę", "On / Off group"),
    ADD_GROUP("Dodaj grupę", "Add group"),
    REMOVE_GROUP("Usuń grupę", "Remove group"),
    DUPLICATE_GROUP("Powiel grupę", "Duplicate group"),
    CLEAR("Wyczyść", "Clear"),
    REMOVE("Usuń", "Remove"),
    // Typy filtrów ------------------------------
    DAYS_OF_WEEK("Dni tygodnia", "Days of week"),
    DAYS_OF_MONTH("Dni miesiąca", "Days of month"),
    DATE_RANGE("Przedział dat", "Date range"),
    TIME_RANGE("Przedział czasu", "Time range"),
    // Dni tygodnia ------------------------------
    MON("Pn", "Mon"),
    TUE("Wt", "Tue"),
    WED("Śr", "Wed"),
    THU("Cz", "Thu"),
    FRI("Pt", "Fri"),
    SAT("Sb", "Sat"),
    SUN("Nd", "Sun"),
    // Jednostki czasu ---------------------------
    SECONDS("sekund", "seconds"),
    MINUTES("minut", "minutes"),
    HOURS("godzin", "hours"),
    DAYS("dni", "days"),
    // Multiselect -------------------------------
    SELECT_ALL_TEXT("Zaznacz wszystkie", "Select all"),
    ALL_SELECTED("Zaznaczono wszystkie", "All selected"),
    COUNT_SELECTED("Zaznaczono # z %%", "# of %% selected"),
    NO_MATCHES_FOUND("Brak rezultatów", "No matches found"),
    // Komunikaty błędów -------------------------
    ERROR("Błąd", "Error"),
    CANT_REMOVE_LAST_GROUP("Nie można usunąć ostatniej grupy", "Can't remove last group"),
    ERROR_PARSING_SCHEDULE("Błąd parsowania harmonogramu", "Error parsing the schedule"),
    UNKNOWN_INTERVAL_UNIT("Nieznana jednostka interwału", "Unknown interval unit");

    // <editor-fold defaultstate="collapsed">
    private LSchedule(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
}

package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Filter")
public enum LFilter implements LString {

    // ------------------- WARUNKI FILTRÓW ------------------- //
    COND__EQUAL("Równe", "Equal"),
    COND__NOT_EQUAL("Różne", "Not equal"),
    COND__ENDS_WITH("Kończy się na", "Ends with"),
    COND__STARTS_WITH("Rozpoczyna się od", "StartsWith"),
    COND__CONTAINS("Zawiera", "Contains"),
    COND__NOT_CONTAIN("Nie zawiera", "Not contain"),
    COND__SIMILAR("Podobne", "Similar"),
    COND__SIMILAR_20("Podobne 20%%", "Similar 20%%"),
    COND__SIMILAR_50("Podobne 50%%", "Similar 50%%"),
    COND__SIMILAR_80("Podobne 80%%", "Similar 80%%"),
    COND__IS_EMPTY("Jest puste", "Is empty"),
    COND__IS_NOT_EMPTY("Nie jest puste", "Is not empty"),
    COND__GREATER("Większe", "Greater"),
    COND__LESS("Mniejsze", "Less"),
    COND__GREATER_OR_EQUAL("Większe lub równe", "Greater or equal"),
    COND__LESS_OR_EQUAL("Mniejsze lub równe", "Less or equal"),
    COND__OLDER_THAN("Starsze niż", "Older than"),
    COND__OLDER_THAN_MINUTES("Starsze niż minuty", "Older than minutes"),
    COND__OLDER_THAN_HOURS("Starsze niż godziny", "Older than hours"),
    COND__OLDER_THAN_DAYS("Starsze niż dni", "Older than days"),
    COND__OLDER_THAN_MONTHS("Starsze niż miesiące", "Older than months"),
    COND__OLDER_THAN_YEARS("Starsze niż lata", "Older than years"),
    COND__YOUNGER_THAN("Młodsze niż", "Younger than"),
    COND__YOUNGER_THAN_MINUTES("Młodsze niż minuty", "Younger than minutes"),
    COND__YOUNGER_THAN_HOURS("Młodsze niż godziny", "Younger than hours"),
    COND__YOUNGER_THAN_DAYS("Młodsze niż dni", "Younger than days"),
    COND__YOUNGER_THAN_MONTHS("Młodsze niż miesiące", "Younger than months"),
    COND__YOUNGER_THAN_YEARS("Młodsze niż lata", "Younger than years"),
    COND__OR_EQUAL("lub równe", "or equal"),
    COND__MINUTES("minuty", "minutes"),
    COND__HOURS("godziny", "hours"),
    COND__DAYS("dni", "days"),
    COND__MONTHS("miesiące", "months"),
    COND__YEARS("lata", "years"),
    // ------------------- KOMUNIKATY BŁĘDÓW ------------------- //
    FILTER_NOT_SPECIFIED("Nie podano filtra", "Filter not specified"),
    CONDITION_NOT_SPECIFIED("Nie podano warunku", "Condition not specified"),
    FILTER_DOESNT_SUPPORT_CONDITION("Filtr \"%1\" nie obsługuje warunku \"%2\"",
            "Filter \"%1\" doesn't support contistion \"%2\""),
    PARSE_FILTERS_ERROR("Błąd parsowania filtrów", "Parse filters error"),
    FILTER_NOT_FOUND("Nie znaleziono filtra \"%1\"", "Filter \"%1\" not found"),
    UNSUPPORTED_FILTER_CONDITION("Warunek filtrowania \"%1\" nie jest wspierany",
            "Unsupported filter condition: \"%1\""),
    FUNCTION_ARG_IS_NULL("Argument funkcji jest null-em", "Function's argument is null"),
    FILTER_RESULTS_LIMITED("Liczba rezultatów została ograniczona do %1 rekordów",
            "Number of results has been limited to %1 records"),
    // -------------------------- INNE ------------------------- //
    DO_FILTER("Filtruj", "Filter"),
    FILTER("Filtr", "Filter"),
    FILTERS("Filtry", "Filters"),
    SHOW("Pokaż", "Show"),
    ENTRIES("pozycji", "Entries"),
    // -------------------------- UI --------------------------- //
    SEARCH("Szukaj", "Search"),
    CLEAR("Wyczyść", "Clear"),
    DUPLICATE_GROUP("Powiel grupę", "Duplicate group"),
    ON_OFF_GROUP("Wł / Wył grupę", "On / Off group"),
    REMOVE_GROUP("Usuń grupę", "Remove group"),
    REMOVE_RECORD("Usuń wiersz", "Remove record"),
    REMOVE("Usuń", "Remove"),
    PROFILE("Profil", "Profile"),
    ADD("Dodaj", "Add"),
    SAVE("Zapisz", "Save"),
    TRUE("Prawda", "True"),
    FALSE("Fałsz", "False"),
    REFRESH("Odśwież", "Refresh"),
    SET_NEW_PROFILE_NAME("Podaj nazwę nowego profilu", "Set new profile name"),
    CANT_DISABLE_LAST_GROUP("Nie można zablokować ostatniej grupy filtrów!", "Can't disable last filter group!"),
    CANT_DELETE_LAST_GROUP("Nie można usunąć ostatniej grupy filtrów!", "Can't remove last filter group!"),
    PROFILE_EXISTS("Profil już istnieje", "Profile already exists");

    //##########################################################################
    // <editor-fold defaultstate="collapsed">
    private LFilter(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

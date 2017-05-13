package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Exceptoins")
public enum LExceptions implements LString {

    ERROR("Błąd", "Error"),
    UNEXPECTED_ERROR("Wystąpił nieoczekiwany bład", "Unexpected error occurred"),
    QUERY("Zapytanie", "Query"),
    LNotFound("Nie znaleziono", "Not found"),
    LServiceError("Błąd usługi", "Service error"),
    ACCESS_DENIED("Brak dostępu", "Access denied"),
    UNAUTHORIZED("Brak autoryzacji", "Unauthorized"),
    FORBIDDED("Foirbidden", "Zabronionu"),
    SERVICE_ERROR("Błąd usługi", "Service error"),
    SERVICE_EXCEPTION("Wyjątek usługi", "Service exception");

    // <editor-fold defaultstate="collapsed">
    private LExceptions(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

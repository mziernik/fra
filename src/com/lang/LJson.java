package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "json")
public enum LJson implements LString {

    ARRAY("tablica", "array"),
    OBJECT("object", "obiekt"),
    VALUE("wartość", "value"),
    NULL("<null>", "<null>"),
    INVALID_ELEMENT_TYPE("Nieprawidłowy typ elementu", "Invalid element type"),
    PASSED_EXPECTED_ARGUMENT("Aktualny: %1,\nOczekiwany: %2", "Passed: %1,\nExpected: %2"),
    PASSED_ARGUMENT("Aktualny: %1", "Passed: %1"),
    EXPECTED_ARGUMENT("Oczekiwany: %1", "Expected: %1"),
    ELEMENT_NOT_FOUND("Nie znaleziono elementu: %1", "Element not found: %1"),
    INVALID_VALUE("Nieprawidłowa wartość \"%1\"", "Invalid value \"%1\""),
    INVALID_FILE_FORMAT("Nieprawidłowy format pliku", "Invalid file format");

    // <editor-fold defaultstate="collapsed">
    private LJson(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}

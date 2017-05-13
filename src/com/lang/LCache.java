package com.lang;

import com.lang.core.*;

@LangDict(name = "Cache")
public enum LCache implements LString {

    CANT_DELETE_FILE_UNTIL_REQUEST_IS_PROCESSING("Nie można usunąć pliku %1 (%2) dopóki przetwarzane jest żądanie",
            "Can't delete the file %1 (%2) until request is processing"),
    CANT_DELETE_FILE_UNTIL_SESSION_IS_ACTIVE("Nie można usunąć pliku %1 (%2)  dopóki trwa sesja %3 ",
            "Can't delete the file %1 (%2) until the session is active %3"),
    REMAINING_TIME("Pozostały czas", "Remaining time"),
    SESSION("Sesja", "Session");

    // <editor-fold defaultstate="collapsed">
    private LCache(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

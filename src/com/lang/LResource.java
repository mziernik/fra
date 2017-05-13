package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "resource")
public enum LResource implements LString {

    INVALID_FILE_FORMAT("Nierozpoznany format pliku: %1", "Invalid file format: %1"),
    INVALID_INDEX("Nieprawidłowy zapis elementu zasobów indeksu", "Invalid index"),;

    // <editor-fold defaultstate="collapsed">
    private LResource(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

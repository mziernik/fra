package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Event")
public enum LEvents implements LString {

    MISSING_TYPE_DECLATATION_OF_GENERIC_CLASS("Brak deklaracji typu generycznego klasy %1",
            "Missing type declaration of a generic class %1"),
    MODIFIED("Zmodyfikowano", "Modified"),
    REMOVED("Usunięto", "Removed"),
    CREATED("Utworzono", "Created"),;

    // <editor-fold defaultstate="collapsed">
    private LEvents(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

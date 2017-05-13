package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Extensions")
public enum LExtensions implements LString {

    INVALID_ADDRESS("Nieprawidłowy adres: %1", "Invalid address: %1"),
    NONE("Brak", "None"),
    TO("Do", "To"),
    CC("Kopia", "Carbon copy"),
    BCC("Ukryta kopia", "Blind carbon copy"),
    XMPP_CONNECTION("Łączenie z serwerem XMPP", "Connectiong to XMPP server"),;

    // <editor-fold defaultstate="collapsed">
    private LExtensions(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

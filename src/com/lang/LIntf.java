package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Intf")
public enum LIntf implements LString {

    TRUE("Tak", "Yes"),
    FALSE("Nie", "False"),
    AUTO("Auto", "Auto");

    // <editor-fold defaultstate="collapsed">
    private LIntf(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

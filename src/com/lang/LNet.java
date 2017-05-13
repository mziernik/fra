package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.utils.Utils;
import com.utils.Is;
import java.util.HashMap;
import com.lang.core.Language;
import com.lang.core.Languages;
import java.util.Map;

@LangDict(name = "Intf")
public enum LNet implements LString {

    INVALID_ENCODING("Nieobsługiwane kodowanie treści: %1", "Invalid encodeing: %1"),;

    // <editor-fold defaultstate="collapsed">
    private LNet(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

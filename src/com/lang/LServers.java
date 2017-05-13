package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "servers")
public enum LServers implements LString {

    SCHEME_NOT_DEFINED("Parametr scheme nie jest zdefiniowany", "Scheme does not defined"),
    PARAMETR_MISSING("Brak parametru: %1", "Missing parameter: %1"),
    WEBAPP_SERVER_LIB_NOT_FOUND("Nie znaleziono bibliotek serwera webapp: tomcat lub jetty",
            "Webapp server libs not found: tomcat nor jetty"),;

    // <editor-fold defaultstate="collapsed">
    private LServers(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

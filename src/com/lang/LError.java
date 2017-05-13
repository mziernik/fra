package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

/**
 * Enumerata zawierająca definiecje tłumaczeń. Zasady kompozycji: - każda
 * enumerata reprezentuje ogólny blok funkcji lub klas (np pakiet) - wartości
 * mogą zawierać nazwy grup (rozdzielone podwójnym podkreślnikiem - jak w sql-u)
 * - nazwy wartości wielimi litermai - wartości ogólne zapisujemy na początu
 * pliku, dedykowane w dalszej części - wartości moga zawierać argumenty %1...%9
 */
@LangDict(name = "Error")
public enum LError implements LString {

    ;
    
    // <editor-fold defaultstate="collapsed">
    private LError(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

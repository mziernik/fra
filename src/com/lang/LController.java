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
@LangDict(name = "Controller")
public enum LController implements LString {

    DATA_UPDATED("Zaktualizowano dane", "Data has been updated"),
    ROLES("Role", "Roles"),
    SAVE("Zapisz", "Save"),
    //--------------------------------------------------------------------------
    USER__CREATE_USER("Tworzenie użytkownika", "Create user"),
    USER__EDIT_USER("Edycja użytkownika", "Edit user"),
    USER__USER_ADDED("Dodano użytkownika %1", "User %1 added"),
    USER__USER_UPDATED("Zaktualizowano użytkownika %1", "User %1 updated"),
    USER__GROUPS("Grupy", "Groups"),
    USER__USERNAME("Użytkownik", "User"),
    USER__DISPLAY_NAME("Nazwa wyświetlana", "Display name"),
    USER__FIRSTNAME("Imię", "Firstanem"),
    USER__LASTNAME("Nazwisko", "Lastname"),
    USER__EMAIL("E-mail", "E-main"),
    USER__TYPE("Typ", "Type"),
    USER__STATUS("Status", "Status"),
    USER__TOKEN("Token", "Token"),
    USER__LDAP("LDAP", "LDAP"),
    //=====================================
    LOGIN__AUTHORIZATION("Autoryzacja", "Authorization"),
    LOGIN__SIGN_IN("Logowanie", "Sign in"),
    LOGIN__USERNAME("Użytkownik", "User name"),
    LOGIN__PASSWORD("Hasło", "Password"),
    LOGIN__DO_LOGIN("Zaloguj", "Login");

    // <editor-fold defaultstate="collapsed">
    private LController(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

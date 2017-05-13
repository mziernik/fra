package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Webapi")
public enum LWebapi implements LString {

    ACCOUNTS("Konta", "Accounts"),
    ACTIVE("Aktywny", "Active"),
    ACTIVITY("Aktywność", "Activity"),
    ADDES("Dodano", "Added"),
    CALLS("Połączenia", "Calls"),
    CONTACT_BOOK("Książka adresowa", "Contact book"),
    CREATED("Utworzona", "Created"),
    EMAIL("e-mail", "e-mail"),
    FIRSTNAME("Imię", "First name"),
    GROUPS("Grupy", "Groups"),
    HOST("Host", "Host"),
    ID("ID", "ID"),
    LANGUAGE("Język", "Language"),
    LASTNAME("Nazwisko", "Last name"),
    LAST_REQUESTR("Ostatnie żądnie", "Last request"),
    LDAP("LDAP", "LDAP"),
    LOGIN("Login", "Login"),
    NAME("Nazwa", "Name"),
    NAME_DICTIONARY("Słownik imion i nazwisk", "First names and surnames dictionary"),
    NUMBERS("Numery", "Numbers"),
    PASSWORD("Hasło", "Password"),
    PREMIUM_NUMBERS("Numery premium", "Premium numbers"),
    RECEIVED("Odebrano", "Received"),
    REMAINING_TIME("Pozostały czas", "remaining time"),
    REQUEST_COUNT("Ilość źądań", "Request count"),
    SENT("Wysłano", "Sent"),
    SESSIONS("Sesje", "Sessions"),
    STATUS("Status", "Status"),
    TIME_LIMIT("Limit czasu", "Time limit"),
    TYPE("Typ", "Type"),
    USER("Użytkownik", "User"),
    USERS("Użytkownicy", "Users"),
    USERS_MODIFICATIONS("Modyfikacje użytkowników", "User modifications"),
    USER_AGENT("User Agent", "User Agent"),
    USER_ID("Identyfikator\nużytkownika", "User ID"),
    WEBSOCKET_CONNECTIONS("Połączenia WebSocket", "WebSocket connections"),;

    // <editor-fold defaultstate="collapsed">
    private LWebapi(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "User")
public enum LUser implements LString {

    ACCEPT("Zastosuj", "Accept"),
    ACCESS_DENIED("Brak dostępu", "Bark dostępu"),
    ACTIVE("Aktywny", "Active"),
    ADD("Doda", "Add"),
    ADMIN("Administrator", "Administrator"),
    APPLY("Zastosuj", "Accept"),
    ATTRIBUTES("Atrybuty", "Attributes"),
    AUTHORIZATION("autoryzacja", "authorization"),
    CACHE("Zarządzanie cache-m", "Cache management"),
    CHANGE_USER_PASSWORD("Zmiana hasła użytkownika %1", "Change user %1 password"),
    CLOSE("Zamknij", "Close"),
    CONFIG_MANAGE("Konfiguracja usługi", "Service configuration"),
    CONFIRM_USER_DELETE("Czy na pewno usunąć użytkownika \"%1\"?", "Do you want to delete user \"%1\"?"),
    CREATE_USER_PASSWORD("Tworzenie hasła użytkownika %1", "Creating password for user %1"),
    CRON_MANAGE("Zarządzanie harmonogramem zadań", "Cron management"),
    DATA("Dane", "Data"),
    DISPLAY_NAME("Nazwa wyświetlana", "Display name"),
    EDIT_USER("Edycja użytkownika", "Edit user"),
    EMAIL("E-mail", "E-mail"),
    FIRST_NAME("Imie", "Name"),
    FORBIDDEN("Dostęp zabroniony", "Access denied"),
    GROUPS("grupy", "groups"),
    GROUPS2("Grupy", "Groups"),
    GROUP_ALREADY_EXISTS("Grupa \"%1\" już istnieje!", "Group \"%1\", already exists"),
    ID_CANT_BE_EMPTY("Identyfikator nie może być pusty", "ID can't be empty"),
    INCORRECT_CHARS_IN_NAME("Nazwa \"%1\" zawiera niedozwolone znaki (%2)", "Name \"%1\" cotains prohibited characters (%2)"),
    INCORRECT_PASSWORD("Nieprawidłowe hasło", "Incorrect password"),
    INCORRECT_USERNAME_OR_PASSWORD("Nieprawidłowa nazwa użytkownika lub hasło", "Invalid username or password"),
    LACK("<brak>", "<lack>"),
    LDAP_USER_NOT_FOUND("Nie znaleziono użytkownika %1 w katalogu LDAP", "Can't find user %1 in LDAP directory"),
    LOGIN("Login", "Login"),
    LOGIN_CANT_BE_EMPTY("Login nie może być pusty", "Login can't be empty"),
    LOGOUT("Wyloguj", "Logout"),
    NAME("Nazwa", "Name"),
    NAME_CANT_BE_EMPTY("Nazwa \"%1\" nie może być pusta", "Name \"%1\" can't be empty"),
    NAME_CANT_CONTAINS_SPACES("Nazwa \"%1\" nie może zawierać spacji", "Name \"%1\" musn't contain spaces"),
    NAME_TO_LONG("Nazwa \"%1\" nie może przekraczać %2 znaków", "Name \"%1\" can't exceed %2 characters"),
    NEW_SESSION("Nowa sesja", "New session"),
    NEW_USER("Nowy użytkownik", "New user"),
    PASSWORD("Hasło", "Password"),
    PASSWORDS_MUST_BE_EQUAL("Hasła muszą być identyczne", "Passwords must be identical"),
    PASSWORD_CANT_BE_EMPTY("Hasło nie może być puste", "Password can't be empty"),
    PASSWORD_CANT_BE_LESS_THAN("Hasło nie może być krótsze niż %1 znaki", "Passwort can't be shorter than %1 characters"),
    PASSWORD_REPEAT("Powtórz hasło", "Repeat password"),
    PERMISSIONS("Uprawnienia", "Permissions"),
    REGISTRATION("Rejestraca", "Registration"),
    REMOVE("Usuń", "Remove"),
    RIGHTS_MANAGE("Zarządzanie uprawnieniami", "Permissions management"),
    ROLE("Rola", "Role"),
    ROLES("Role", "Roles"),
    SERVICE_DB_MANAGE("Zarządzanie bazą serwisową", "Database management"),
    SESSION("Sesja: %1", "Session: %1"),
    STATUS_PREVIEW("Podgląd statusu usługi", "Service status preview"),
    SURNAME("Nazwisko", "Surname"),
    TESTS("Testy", "Tests"),
    UNAUTHORIZED("Brak autoryzacji", "Unauthorized"),
    UPDATE("Aktualicacja", "Update"),
    USER("Użytkownik", "User"),
    USERS_MANAGE("Zarządzanie użytkownikami", "User management"),
    USER_ALREADY_EXIST("Użytkownik \"%1\" już istnieje", "User \"%1\" already exists"),
    USER_GENITIVE("użytkownika", "user"),
    USER_GROUP_REMOVED("Usunięto grupę \"%1\"", "Group \"%1\" deleted"),
    USER_GROUP_TYPE__STANDARD("Standardowa", "Standard"),
    USER_GROUP_UPDATED("Zaktualizowano grupę \"%1\"", "Group \"%1\" updated"),
    USER_NOT_ACTIVE("Użytkownik nieaktywny", "Inactive user"),
    USER_NOT_FOUND("Nie znaleziono użytkownika", "User not found"),
    USER_NOT_FOUND_ARG("Nie znaleziono użytkownika \"%1\"", "User \"%1\" not found"),
    USER_STATUS__EMAIL_VERIFICATION("Weryfikacja adresu e-mail", "E-mail address verification"),
    USER_STATUS__INACTIVE("Nieaktywny", "Inactive"),
    USER_STATUS__MODERATION("Moderacja", "Moderation"),
    USER_STATUS__REMOVED("Usunięty", "Deleted"),
    USER_TYPE__API("API", "API"),
    USER_TYPE__STANDARD("Standardowy", "Standard"),
    USER_TYPE__VIRTUAL("Wirtualny", "VIRTUAL"),
    WRONG_USER_GROUP_TYPE("Nieprawidłowy typ grupy: \"%1\"", "Incorrect group type \"%1\""),
    WRONG_USER_TYPE("Nieprawidłowy typ użytkownika: \"%1\"", "Incorrent user type \"%1\""),;

    // <editor-fold defaultstate="collapsed">
    private LUser(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}

package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Context")
public enum LContext implements LString {

    APPCONFIG_UNDEFINED_KEY("Klucz aplikacji nie jest zdefiniowany", "Undefined application key"),
    APPCONFIG_LACK_OF_WEBLISTENER_ANNOTATION("Klasa %1 nie posiada adnotacji javax.servlet.annotation.WebListener",
            "Class %1 doesn't have javax.servlet.annotation.WebListener annotation"),
    APPCONFIG_LACK_OF_CTOR_REQUIRED_IN_SERVLET("Klasa %1 nie posiada konstruktora nie przyjmującego parametrów, wymaganego w serwletach",
            "Class %1 doesn't have a proper contructor required in servlets"),
    APPCONFIG_LACK_OF_CTOR_STR_ARRAY("Klasa %1 nie posiada konstruktora (String[] args)",
            "Class %1 doesn't have a constructor(String[] args)"),
    APPCONFIG_UNKNOWN_SERVICE_ID("Niezdefiniowany identyfikator usługi", "Unknown service ID"),
    APPCONFIG_CALL_SET_SERVICE_NAME("Należy wywołać metodę AppConfig.setServiceName() przed utworzeniem kontekstu\n(np przez przeciążenie metody %1.onInitialize()",
            "Call AppConfig.setServiceName() before creating context (for example by overloading method %1.onInitialize()"),
    APPCONFIG_CALL_SET_SERVICE_TITLE("Należy wywołać metodę AppConfig.setServiceTitle() przed utworzeniem kontekstu\n(np przez przeciążenie metody %1.onInitialize()",
            "Call AppConfig.setServiceTitle() before creating context (for example by overloading method %1.onInitialize()"),
    APPCONFIG_INVALID_HOME_DIR("Katalog domowy %1 nie może wskazywać na katalog projektu. Wskaż katalog \"bin\" jako katalog roboczy",
            "Home directory %1 can not point to the project directory. Specify the  \"bin\" as the working directory"),
    // ---
    APPCONTEXT_CANT_RUN_MULTIPLE_INSTANCES("Nie można uruchomić wielu instancji jednocześnie",
            "Can't run multiple instances simultaneously"),
    APPCONTEXT_CONTEXT_NOT_INITIALIZED("Kontekst nie został zainicjalizowany", "Context not initialized"),
    APPCONTEXT_READING_CONFIG("Wczytanie konfiguracji", "Reading configuration"),
    APPCONTEXT_INITIALIZING_CONTEXT("Inicjalizacja kontekstu", "Initializing context"),
    // ---
    APPCONTEXTINIT_CONTEXT_INITIALIZED("Zainicjalizowano kontekst \"%1\", czas %2",
            "Context \"%1\" initialized, time: %1"),
    // --
    MAINTAIN_BACKUP_DIR("Kopia zapasowa plików z katalogu \"etc\"", "Backup files from \"etc\" directory"),;

    // <editor-fold defaultstate="collapsed">
    private LContext(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}

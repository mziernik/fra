package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Servlet")
public enum LServlet implements LString {

    ARG_MUST_HAVE_ANNOTATION("Argument %1 metody %2 powinien posiadać adnotację @Arg", "Argument %1 of method %2 must have @Arg annotation"),
    CLASS("Klasa", "Class"),
    CLASS_MUST_BE_STATIC("Klasa %1 musi być statyczna", "Class %1 must be static"),
    CLASS_NOT_FOUND("Nie znaleźiono klasy %1", "Class %1 not found"),
    CONNECTION_LIMIT_EXCEEDED("Przekroczono limit połączeń", "Connection limit exceeded"),
    CONTEXT_PATHS_MISMATCH("Niezgodność ścieżek kontekstu", "Context paths mismatch"),
    CONTROLLED_NOT_INITIALIZED_YET("Kontroler %1 nie został jeszcze zainicjalizowany", "Controller %1 has't been initialized yet"),
    CONTROLLER_DEF_NOT_FOUND("Nie znaleziono definicji kontrolera %1", "Controller definition not found: %1"),
    CONTROLLER_REQUEST_NOT_FOUND("Nie znaleziono żądania kontrolera", "Controller request not found"),
    CURRENT_THREAD_BUSY("Bieżący wątek obsługuje już żądanie %1", "Current thread %1 is handling request now"),
    ENDPOINT_NAME_DUPLICATED("Zduplikowana nazwa endpoint-u", "Endpoint name duplicated"),
    FILE_NOT_ADDED("Nie dodano pliku", "Fine not added"),
    HANDLER_DEF_NOT_FOUND("Wymagana definicja handlera  %1", "Handler definition %1 not found"),
    INCORRECT_CALL("Nieprawidłowe wywołanie", "Incorrect call"),
    INIT_JAX_WS_MODULE("Inicjalizacja modułu JAX-WS", "Initialize JAX-WS module"),
    INIT_SERVLET("Inicjalizacja servletu", "Servlet is initializing"),
    INVALID_GET_LAMBDA_ARG("Nieprawidłowe argumenty metody get$Lambda", "Invalid argument passed to get$Lambda method"),
    INVALID_ON_REQUEST_ARG("Nieprawidłowe argumenty metody onRequest", "Invalid argument passed to onRequst() method"),
    LIB_NOT_FOUND("Nie znaleźiono biblioteki %1", "Library %1 not found"),
    MASTER_CONTROLLER_DEF_NOT_FOUND("Brak definicji kontrolera nadrzędnego dla %1", "No definition of the master controller for %1"),
    MEDODA_MUST_RETURN_VOID("Metoda %1.%2 musi być tupu void", "Method %1.%2 have to returns void"),
    METHOD_NOT_FOUND("Nie znaleziono metody %1.%2", "Method not found %1.%2"),
    REQUENST_NOT_INITIALIZED_BY_AJAX("Żądanie nie zostało zainicjowane przez Ajax-a", "Request wasn't initialized by Ajax"),
    THREAD_HANDLER_NOT_FOUND("Nie znaleziono handlera wątku: %1 ", "Thread hadler not found: %1"),
    VIEW_CONTROLLER_NOT_FOUND("Nie znaleziono kontrolera widoku %1", "View controller not found %1"),;

    // <editor-fold defaultstate="collapsed">
    private LServlet(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}

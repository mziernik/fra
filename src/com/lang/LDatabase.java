package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Database")
public enum LDatabase implements LString {

    NO_DEFINED_TABLES("Brak zdefiniowanych tabel", "There are no defined tables"),
    UNSUPPORTED_FORMAT("Format nie jest jeszcze obsługiwany", "Unsupported format"),
    NOT_TEMPORARY_ELEMENT("Element nie jest tymczasowy", "Element is not a temporary"),
    INVALID_CTOR_CALL("Nieprawidłowe wywołanie konstruktora klasy %1",
            "Invalid class contructor call %1"),
    LACK_OF_OBJECT_KEY("Brak klucza obiektu", "Object key not found"),
    ELEMENT_EXISTS("Element %1 juz istnieje", "Element %1 alerady exists"),
    ELEMENT_NOT_FOUND("Nie znaleziono elementu %1", "Element %1 not found"),
    FUNCTION("Funkcja %1", "Function %1"),
    CREATING_STRUCTURE_PACKAGE("Generuję strukturę, pakiet %1", "Generating structure, package %1"),
    LACK_OF_PRIMARY_KEY("Brak definicji klucza głównego", "Primary key not found"),
    LACK_OF_COLUMN_NAME("Brak nazwy kolumny", "Column name not found"),
    LACK_OF_COLUMN_DEFINITIONS("Brak zdefiniowanych kolumn", "Lack of column definitions"),
    INVALID_NUMBER_OF_ARGS("Nieprawidłowa ilość argumentów.\nWymagane %1, aktualnie %2",
            "Invalid number of arguments.\nRequired %1 , passed %2"),
    LACK_OF_COLUMN_VALUE("Brak zdefiniowanej wartości kolumny \"%1\"",
            "Lack of column value \"%1\""),
    EMPYT_PRIMARY_KEY("Wartość klucza głównego nie może być pusta",
            "The value of the primary key can not be empty"),
    PRIMARY_KEY_DUPLICATED("Zduplikowany klucz główny (%1)", "Primary key duplicated (%1)"),
    DATABASE("Baza danych", "Database"),
    TRANSACTION_ALREADY_STARTED("Transakcja została już rozpoczęta", "Transaction has been already started"),
    TRANSACTION_NOT_STARTED("Transakcja nie została rozpoczęta", "Transaction hasn't been started"),
    LINE("Linia", "Line"),
    MESSAGES("Komunikaty", "Messages"),
    RESULT("Resultat", "Result"),
    LACK_OF_DEFAULT_CONNECTION_PROFIL("Brak zdefiniowanego domyślnego profilu połączenia bazą danych",
            "Default database connection profil not found"),
    LACK_OF_DEFAULT_CONNECTION("Brak zdefiniowanego domyślnego połączenia",
            "Default database connetion not found"),
    QUERY_NOT_FOUND("Nie znaleziono zapytania \"%1\"", "Query \"%1\" not found"),
    COLUMN_NOT_FOUND("Nie znaleziono kolumny \"%1\"", "Column \"%1\" not found"),
    RECORD_CONTAINS_NULL("Rekord \"%1\" ma wartość NULL", "Record \"%1\" contains NULL value"),
    INVALID_BOOL_VALUE("Nieprawidłowa wartość typu boolean", "Invalid bool value"),
    PERHAPS_YOU_MEAN("Może chodziło Ci o %1?", "Perhaps you mean %1?"),;

    // <editor-fold defaultstate="collapsed">
    private LDatabase(String pl, String en) {
        Languages.addLstringEntry(this, Languages.pl, pl);
        Languages.addLstringEntry(this, Languages.en, en);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

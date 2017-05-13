package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Util")
public enum LUtil implements LString {

    ARGS("Argumenty", "Arguments"),
    ARGS_CONFLICT("Konflikt argumentów", "Arguments conflict"),
    CANT_DESERIALIZE_CLASS("Nie można zdeserializować klasy %1", "Can't deserialize class %1"),
    CANT_FIND_FIELD("Nie znaleziono pola %1", "Can't find field %1"),
    CANT_HAVE_MODIFIER("nie może posiadać modyfikatora", "can't have any modifier"),
    CLASS("Klasa", "Class"),
    DATE_PARSE_FORMAT_ERROR("Błąd parsowania daty \"%1\".\nWymagany format \"%2\"", "Parse date error \"%1\".\nRequired format \"%2\""),
    DECLARED_OR_CURRENT("Deklarowane: %1\nAktualne: %2", "Declared: %1\n,Current: %2"),
    FIELD("Pole", "Field"),
    HIGH("Wysoki", "High"),
    INCORRECT_ARGS_COUNT("Nieprawidłowa ilość parametrów", "Incorrect arguments count"),
    INVALID_CHAR_VALUE("Nieprawidłowa wartość char", "Invalid char value"),
    INVALID_CHAR_VALUE_ARG("Niedozwolony znak (\"%1\", poz. %2) w wartości \"%3\"", "Invalid character (\"%1\", pos. %2) in value \"%3\""),
    INVALID_ESCAPING("Nieprawidłowe escapowanie \"\\%1\"", "Invalid escaping \"\\%1\""),
    INVALID_VALUE("Nieprawidłowa wartość %1", "Invalid value: %1"),
    INVALID_VALUE_TYPE("Nieprawidłowy typ wartości \"%1\" (%2)", "Invalid value type  \"%1\" (%2)"),
    INVALID_VALUE_VPARSER("Nieprawidłowa wartość \"%1\" (\"%2\")", "Invalid value  \"%1\" (\"%2\")"),
    LACK_OF_CTOR_WITH_ARGS("Brak konstruktora klasy %1 przyjmująćego prametry: %2", "Lack of class constructor %2, which takes arguments: %1"),
    LACK_OF_NAME("Brak nazwy", "Lack of name"),
    LACK_OF_OPERATOR("Brak operatora", "Lack of operator"),
    LACK_OF_VALUE("Brak wartości", "Lack of value"),
    LOW("Niski", "Low"),
    METHOD("Metoda", "Method"),
    METHOD_CANT_TEKE_ARGS("Metoda %1 nie może przyjmować parametrów", "Method %1 can't take arguments"),
    METHOD_ISNT_STATIC("Metoda %1 nie jest statyczne", "Method %1 isn't static"),
    METHOD_IS_ABSTRACT("Metoda %1 jest abstrakcyjna", "Method %1 is abstract"),
    METHOD_MUST_TAKE_ARGS("Metoda %1 musi przyjmować parametry", "Method %1 must take arguments"),
    MUST_HAVE_MODIFIER("musi posiadać modyfikator", "must have a modifier"),
    NO("Nie", "No"),
    NORMAL("Normalny", "Normal"),
    OBJECT_WITHOUT_TOSTRING_OVERWRITED("Obiekt %1 nie ma przeciążonej metody toString", "Object %1 doesn't overwrite toString() method"),
    REALTIME("Czasu rzeczywistego", "Realtime"),
    THE_HIGHEST("Najwyższy", "The highest"),
    THE_LOWEST("Najniższy", "The lowest"),
    UNKNOWN("Nieznany", "Unknown"),
    VALUE_CANT_BE_EMPTY("Wartość nie może być pusta", "Value can't be empty"),
    VALUE_CANT_BE_EMPTY_VPARSE("Wartość \"%1\" nie może być pusta", "Value \"%1\" can't be empty"),
    VALUE_NOT_FOUND_VPARSE("Nie znaleziono wartości \"%1\"", "Value \"%1\" not found"),
    WAITING_TIMEOUT("przekroczony limit czasu oczekiwania", "waiting timeout"),
    YES("Tak", "Yes"),;

    // <editor-fold defaultstate="collapsed">
    private LUtil(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}

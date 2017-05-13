package com.lang;

import com.exceptions.EError;
import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;
import org.xml.sax.SAXParseException;

@LangDict(name = "XML")
public enum LXml implements LString {

    ATTRIBUTE_NOT_FOUND("Nie znaleziono atrybutu", "Attribute not found"),
    ATTRIBUTE_NOT_FOUND_ARG("Nie znaleziono atrybutu \"%1\"", "Attribute \"%1\" not found"),
    ATTRIBUTE_NOT_FOUND_ARG_2("Nie znaleziono atrybutu \"%s\"", "Attribute \"%s\" not found"),
    ATTRIBUTE_VALUE_CANT_BE_EMPTY("Wartość atrybutu \"%s\" nie może być pusta", "Value of attribute \"%s\" can't be empty"),
    BRANCH_NOT_FOUND("Nie znaleziono gałęzi \"%1\"", "Branch \"%1\" not found"),
    COLUMN("kolumna", "column"),
    INVALID_ATTRIBUTE_VALUE("Nieprawidłowa wartość atrybutu \"%s\" (\"%s\")", "Invalid attribute value \"%s\" (\"%s\")"),
    INVALID_ATTRIBUTE_VALUE_TYPE("Nieprawidłowy typ wartość atrybutu \"%s\" (\"%s\")", "Invalid attribute type \"%s\" (\"%s\")"),
    INVALID_NAME("Nieprawidłowa nazwa \"%1\"", "Invalid name \"%1\""),
    LACK_OF_NEEDED_VALUE("Brak wymaganej wartości %1", "Lack of needed value %1"),
    LINE("linia", "line"),
    PARSE_ERROR("Błąd parsowania XML", "XML parse error"),
    VALUE_CANT_BE_EMPTY("Wartość %1 nie może być pusta", "Value %1 can't be empty"),;

    // <editor-fold defaultstate="collapsed">
    private LXml(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

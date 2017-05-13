package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Io")
public enum LIo implements LString {

    VALUE_CANT_BE_LESS_THAN_ZERO("Wartość %1 nie może być mniejsza niż 0",
            "Value %1 can't be less than zero"),
    VALUE_LESS_THAN_ZERO("Wartość %1 mniejsza niż 0", "Value %1 less than zero"),
    LACK_OF_DEFINED_PASSWORD("Brak zdefiniowanego hasła", "Defined password missing"),
    INVALID_CHECKSUM("Błąd weryfikacji sumy kontrolnej", "Checksum is invalid"),
    UNSUPPORTED_DATA_TYPE("Nieobsługiwany typ danych", "Unsupported data type"),
    MULTIPLE_CALL_CHECKSUM_GENERATOR("Wielokrotne wywołanie generatora sumy kontrolnej",
            "Multiple call checksum generator"),
    UNSUPPORTED_OPERATION("Operacja nie obsługiwana", "Unsupported operation"),
    DIRECTORY_NOT_EXIST("Katalog \"%1\" nie istnieje", "Directory \"%1\" does not exist"),
    UNSUPPORTED_PROTOCOL("Nieobsługiwany protokół: %1", "Unsupported protocol: %1"),
    DIRECTORY_ALREADY_EXISTS("Katalog \"%1\" już istnieje!", "Directory \"%1\" already exists"),
    FILE_ALREADY_EXISTS("Plik \"%1\" już istnieje!", "File \"%1\" already exists"),
    FILE_NOT_EXIST("Plik \"%1\" nie istnieje!", "File \"%1\" does not exist"),
    SRC_FILE_NOT_EXIST("Plik źródłowy \"%1\" nie istnieje!", "Source file \"%1\" does not exist"),
    DEST_FILE_ALREADY_EXISTS("Plik docelowy \"%1\" już istnieje!", "Destination file \"%1\" already exists"),
    INVALID_FILE_NAME("Niedozwolona nazwa pliku: \"%1\"", "Invalid file name \"%1\""),
    INVALID_FILE_PATH("Ścieżka \"%1\" do pliku \"%2\" jest nieprawidłowa!", "Path \"%1\" to file \"%1\" is invalid"),
    INVALID_ARCHIVE_FORMAT("Nieprawidłowy format archiwum", "Invalid archive format"),
    INVALID_ZIP_FILE("Nieprawidłowa struktura pliku ZIP", "Invalid ZIP file"),
    INVALID_FILE_SIGNATURE("Nieprawidłowa sygnatura pliku", "Invalid file signature"),
    INVALID_FILE_VERSION("Nieprawidłowa wersja pliku", "Invalid file version"),
    INVALID_FILE_SIZE("Nieprawidłowy rozmiar pliku", "Invalid file size"),
    INVALID_PART_COUNT("Nieprawidłowa liczba części", "Invalid part count"),
    MISSING_KEY("Brak klucza", "Key is missing"),
    INVALID_KEY("Nieprawidłowy klucz", "Invalid key");

    // <editor-fold defaultstate="collapsed">
    private LIo(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}

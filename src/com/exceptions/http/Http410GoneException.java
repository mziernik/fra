package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http410GoneException extends HttpException {
    // zadanie zostalo juz przetworzone i nie jest dostepne

    public Http410GoneException(String message, String details) {
        super(410, message != null ? message : "Zasób niedostępny");
        this.details(details);
    }

    public Http410GoneException() {
        super(410, "Zasób niedostępny");
    }
}

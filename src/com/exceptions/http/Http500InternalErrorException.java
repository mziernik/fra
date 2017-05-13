package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http500InternalErrorException extends HttpException {

    public Http500InternalErrorException(String message, String details) {
        super(500, message != null ? message : "Wewnętrzny błąd");
        this.details(details);
    }

    public Http500InternalErrorException() {
        super(500, "Wewnętrzny błąd");
    }
}

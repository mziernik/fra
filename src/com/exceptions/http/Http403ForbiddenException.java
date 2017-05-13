package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http403ForbiddenException extends HttpException {

    public Http403ForbiddenException(String message) {
        super(403, message);
    }

    public Http403ForbiddenException(String message, String details) {
        super(403, message == null || message.trim().isEmpty() ? "Brak dostępu" : message);
        this.details(details);
    }

    public Http403ForbiddenException() {
        super(403, "Brak dostępu");
    }
}

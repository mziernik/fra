package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http503ServiceUnavailable extends HttpException {

    public Http503ServiceUnavailable(String message, String details) {
        super(503, message != null ? message : "Usługa niedostępna");
        this.details(details);
    }

    public Http503ServiceUnavailable() {
        super(503, "Usługa niedostępna");
    }
}

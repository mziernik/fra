package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class PageException extends HttpException {

    public PageException(int errorNr, String message, String details) {
        super(errorNr, message);
        this.details(details);
    }

    public PageException(String message, String details) {
        super(500, message);
        this.details(details);
    }

    public PageException(int errorNr, String message) {
        super(errorNr, message);
    }

    public PageException(String message) {
        super(500, message);
    }
}

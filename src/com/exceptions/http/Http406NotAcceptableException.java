package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http406NotAcceptableException extends HttpException {

    public Http406NotAcceptableException(String message) {
        super(406, message);
    }

    public Http406NotAcceptableException() {
        super(406, "Nieakceptowane");
    }
}

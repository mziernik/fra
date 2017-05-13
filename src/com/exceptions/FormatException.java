package com.exceptions;

/**
 * @author Miłosz Ziernik
 * @date 30 września 2015
 * @encoding UTF-8
 */
public class FormatException extends RuntimeException {

    public FormatException() {
    }

    public FormatException(String message) {
        super(message);
    }

    public FormatException(Throwable cause) {
        super(cause);
    }

    public FormatException(String message, Throwable cause) {
        super(message, cause);
    }

}

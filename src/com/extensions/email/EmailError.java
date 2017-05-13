package com.extensions.email;

/**
 * @author Miłosz Ziernik
 * @date 18 września 2015
 * @encoding UTF-8
 */
public class EmailError extends Error {

    public EmailError(String message) {
        super(message);
    }

    public EmailError(Throwable cause) {
        super(cause);
    }

    public EmailError(String message, Throwable cause) {
        super(message, cause);
    }

}

package com.exceptions;

import com.exceptions.intf.DetailedException;
import com.exceptions.intf.IHttpException;
import com.lang.LExceptions;

public class AuthenticationException
        extends DetailedException
        implements IHttpException {

    public AuthenticationException() {
        super(LExceptions.UNAUTHORIZED.toString());
    }

    public AuthenticationException(String message) {
        super(message);
    }

    public AuthenticationException(Throwable cause) {
        super(cause);
    }

    public AuthenticationException(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public int getHttpStatus() {
        return 401;
    }

}

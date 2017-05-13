package com.exceptions;

import com.exceptions.intf.DetailedException;

import com.exceptions.intf.IHttpException;
import com.lang.LExceptions;

public class AccessDeniedException
        extends DetailedException
        implements IHttpException {

    public AccessDeniedException() {
        super(LExceptions.ACCESS_DENIED.toString());
    }

    public AccessDeniedException(String message) {
        super(message);
    }

    public AccessDeniedException(Throwable cause) {
        super(cause);
    }

    public AccessDeniedException(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public int getHttpStatus() {
        return 403;
    }

}

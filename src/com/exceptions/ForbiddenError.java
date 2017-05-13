package com.exceptions;

import com.exceptions.intf.*;
import com.lang.LExceptions;

public class ForbiddenError
        extends DetailedError
        implements IHttpException {

    public ForbiddenError() {
        super(LExceptions.FORBIDDED.toString());
    }

    public ForbiddenError(String message) {
        super(message);
    }

    public ForbiddenError(Throwable cause) {
        super(cause);
    }

    public ForbiddenError(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public int getHttpStatus() {
        return 403;
    }

}

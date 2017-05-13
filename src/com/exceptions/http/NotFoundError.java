package com.exceptions.http;

import com.exceptions.intf.*;
import com.lang.LExceptions;

public class NotFoundError
        extends DetailedError
        implements IHttpException {

    public NotFoundError() {
        super(LExceptions.LNotFound.toString());
    }

    public NotFoundError(String message) {
        super(message);
    }

    public NotFoundError(Throwable cause) {
        super(cause);
    }

    public NotFoundError(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public int getHttpStatus() {
        return 403;
    }

}

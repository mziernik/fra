package com.exceptions;

import com.lang.core.LString;
import com.exceptions.intf.*;
import com.lang.LExceptions;
import com.servlet.requests.HttpRequest;
import java.lang.reflect.InvocationTargetException;

public class ServiceError extends DetailedError {

    private LString message;
    private HttpRequest http;
    private Object[] params = new Object[0];

    public ServiceError() {
        this(LExceptions.SERVICE_ERROR.toString());
    }

    public ServiceError http(HttpRequest http) {
        this.http = http;
        return this;
    }

    public ServiceError(LString message, Object... params) {
        this(message, null, params);
    }

    public ServiceError(LString message, Throwable cause, Object... params) {
        super(message.toString(), cause);
        this.message = message;
        if (params != null && params.length > 0)
            this.params = params;
    }

    public ServiceError message(LString message, Object... params) {
        this.message = message;
        this.params = params;
        return this;
    }

    @Override
    public String getLocalizedMessage() {
        return message != null
                ? (http != null
                        ? message.toStringS(http, params)
                        : message.toString(params))
                : super.getLocalizedMessage();
    }

    @Override
    public String getMessage() {
        return message != null
                ? (http != null
                        ? message.toStringS(http, params)
                        : message.toString(params))
                : super.getMessage();
    }

    @Deprecated
    public ServiceError(String message) {
        super(message);
    }

    public ServiceError(Throwable cause) {
        super(cause instanceof InvocationTargetException ? cause.getCause() : cause);
    }

    @Deprecated
    public ServiceError(String message, Throwable cause) {
        super(message, cause);
    }

    public static ServiceError get(Throwable source) {
        return source instanceof RuntimeException
                ? (ServiceError) source : new ServiceError(source);
    }

}

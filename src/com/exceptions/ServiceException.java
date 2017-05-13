package com.exceptions;

import com.lang.core.LString;
import com.exceptions.intf.*;
import com.lang.*;
import com.servlet.requests.HttpRequest;
import java.lang.reflect.InvocationTargetException;

public class ServiceException extends DetailedRuntimeException {

    private LString message;
    private HttpRequest http;
    private Object[] params = new Object[0];

    /**
     * ************************************************************************
     * PRZYKŁAD ZALECANEJ FORMY PRZECHWYTYWANIA WYJĄTKÓW try { throw new
     * IOException("1234"); } catch (IOException ex) { throw
     * ServiceException.get(ex) .details("user", "jan") .details("req: {}"); }
     *
     *************************************************************************
     */
    public ServiceException() {
        this(LExceptions.SERVICE_EXCEPTION.toString());
    }

    public ServiceException http(HttpRequest http) {
        this.http = http;
        return this;
    }

    public ServiceException message(LString message, Object... params) {
        this.message = message;
        this.params = params;
        return this;
    }

    public ServiceException(LString message, Object... params) {
        this(message, null, params);
    }

    public ServiceException(LString message, Throwable cause, Object... params) {
        super(message.toString(), cause);
        this.message = message;
        if (params != null && params.length > 0)
            this.params = params;
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

    public ServiceException(String message) {
        super(message);
    }

    public ServiceException(Throwable cause) {
        super(cause instanceof InvocationTargetException ? cause.getCause() : cause);
    }

    public ServiceException(String message, Throwable cause) {
        super(message, cause);
    }

    public static RuntimeException get(Throwable source) {
        return source instanceof RuntimeException
                ? (RuntimeException) source : new ServiceException(source);
    }

}

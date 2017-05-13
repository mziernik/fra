package com.mlogger;

import com.exceptions.EError;
import com.mlogger.interfaces.ILogEventsHandler;
import com.mlogger.utils._Internal;
import java.util.Collections;
import java.util.logging.Logger;

public abstract class MLoggerMethods extends Logger {

    public MLoggerMethods() {
        super("mlogger", null);
    }

    public Log log(Object value) {
        return log(null, value, null);
    }

    public Log log(String tag, Object value) {
        return log(tag, value, null);
    }

    public Log log(String tag, Object value, Object details) {

        return new Log(LogKind.LOG, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    public Log trace(Object value) {
        return trace(null, value, null);
    }

    public Log trace(String tag, Object value) {
        return trace(tag, value, null);
    }

    public Log trace(String tag, Object value, Object details) {
        return new Log(LogKind.TRACE, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    public Log info(Object value) {
        return info(null, value, null);
    }

    public Log info(String tag, Object value) {
        return info(tag, value, null);
    }

    public Log info(String tag, Object value, Object details) {
        return new Log(LogKind.INFO, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    public Log debug(Object value) {
        return debug(null, value, null);
    }

    public Log debug(String tag, Object value) {
        return debug(tag, value, null);
    }

    public Log debug(String tag, Object value, Object details) {
        return new Log(LogKind.DEBUG, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    public Log warning(Object value) {
        return warning(null, value, null);
    }

    public Log warning(String tag, Object value) {
        return warning(tag, value, null);
    }

    public Log warning(String tag, Object value, Object details) {
        return new Log(LogKind.WARNING, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    public Log warning(Throwable ex) {
        return warning(null, ex, null);
    }

    public Log warning(String tag, Throwable ex) {
        return warning(tag, ex, null);
    }

    public Log warning(String tag, Throwable ex, Object details) {

        return new Log(LogKind.WARNING, (MLogger) this)
                .tag(tag)
                .details(details)
                .setErrorStackTrace(ex)
                .setExceptionDetails(ex)
                .setException(ex)
                .send();
    }

    public Log error(Object value) {
        return error(null, value, null);
    }

    public Log error(String tag, Object value) {
        return error(tag, value, null);
    }

    public Log error(String tag, Object value, Object details) {
        return new Log(LogKind.ERROR, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    // Exception lub warning
    public Log error(Throwable ex) {
        return new Log(new EError(ex).critical ? LogKind.ERROR : LogKind.WARNING,
                (MLogger) this)
                .setErrorStackTrace(ex)
                .setExceptionDetails(ex)
                .setException(ex)
                .setSourceCode(ex)
                .send();
    }

    /*
    public Log exception(Throwable ex) {
        return new Log(LogKind.EXCEPTION, (MLogger) this)
                .setErrorStackTrace(ex)
                .setExceptionDetails(ex)
                .setException(ex)
                .setSourceCode(ex)
                .send();
    }
     */
    public Log error(String tag, Throwable ex) {
        return error(tag, ex, null);
    }

    public Log error(String tag, Throwable ex, Object details) {
        return new Log(LogKind.ERROR, (MLogger) this)
                .details(details)
                .setErrorStackTrace(ex)
                .setExceptionDetails(ex)
                .setException(ex)
                .tag(tag)
                .send();
    }

    public Log event(Object value) {
        return event(null, value, null);
    }

    public Log event(String tag, Object value) {
        return event(tag, value, null);
    }

    public Log event(String tag, Object value, Object details) {
        return new Log(LogKind.EVENT, (MLogger) this)
                .tag(tag)
                .value(value)
                .details(details)
                .send();
    }

    /**
     * Loguj biezaca procedure
     */
    public Log currentMethod() {

        StackTraceElement[] stel = Thread.currentThread().getStackTrace();
        if (stel.length < 3)
            return null;

        StackTraceElement ste = stel[2];
        String proc = ste.getClassName() + "." + ste.getMethodName()
                + " (" + ste.getFileName() + ":" + ste.getLineNumber() + ")";

        return new Log(LogKind.DEBUG, (MLogger) this)
                .tag("Method")
                .value(proc)
                .send();
    }

}

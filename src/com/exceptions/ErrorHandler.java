package com.exceptions;

import com.config.CService;
import com.context.AppContext;
import com.context.index.Index;
import com.lang.LExceptions;
import com.servlet.controller.Controller;
import com.servlet.page.ErrorPage;
import com.servlet.requests.HttpRequest;

public class ErrorHandler {

    public void onException(HttpRequest req, Throwable ex) {
        new ErrorPage().processException(ex, req, req.request, req.response,
                Controller.getInstance(false));
    }

    public int onGetHttpStatus(Throwable exception, int httpStatus) {
        return httpStatus;
    }

    /**
     * Zwraca element stosu błędów, który jest prawdopodobną przyczyną błędu
     *
     * @param ex
     * @return
     */
    public StackTraceElement getExceptionReason(Throwable ex) {
        if (ex == null)
            return null;

        while (ex.getCause() != null)
            ex = ex.getCause();

        StackTraceElement[] stack = ex.getStackTrace();

        for (StackTraceElement el : stack)
            try {
                Class<?> cls = Class.forName(el.getClassName(), false,
                        ClassLoader.getSystemClassLoader());
                if (!Index.svrIdx.files.contains(cls.getName().replace(".", "/") + ".class"))
                    continue;

                if (el.getFileName() != null && el.getLineNumber() > 0)
                    return el;
            } catch (Throwable e) {
            }

        for (StackTraceElement el : stack)
            try {
                Class<?> cls = Class.forName(el.getClassName(), false,
                        ClassLoader.getSystemClassLoader());
                if (!Index.fraIdx.files.contains(cls.getName().replace(".", "/") + ".class"))
                    continue;

                if (el.getFileName() != null && el.getLineNumber() > 0)
                    return el;
            } catch (Throwable e) {
            }
        return null;
    }

    /**
     * Metoda formatuje i zwraca szczegóły błędu (uzaleznione od typu błedu i
     * trybu pracy usługi)
     *
     * @param ex
     * @return
     */
    public ErrorMessage getMessage(Throwable ex) {

        boolean details = CService.devMode() || CService.testMode();

        EError err = new EError(ex);

        ErrorMessage msg = new ErrorMessage(err,
                !details,
                err.critical,
                details ? err.shortClasses.toString(", ") : LExceptions.ERROR.toString(),
                details || ex instanceof Error
                        ? err.toString(false)
                        : LExceptions.UNEXPECTED_ERROR.toString());

        if (details)
            msg.details.putAll(err.details);

        return msg;
    }

}

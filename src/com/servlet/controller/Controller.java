package com.servlet.controller;

import com.exceptions.CoreException;
import com.lang.LServlet;
import com.servlet.Header;
import com.servlet.MainServlet.ControllersMap;
import com.servlet.handlers.HRequests;
import com.servlet.interfaces.HttpMethod;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.RequestParams;
import com.user.BaseUserData;
import com.utils.collections.Strings;
import com.utils.reflections.TMethod;

public abstract interface Controller {

    public default HttpRequest http() {
        return endpoint().http;
    }

    /**
     * Zwraca bieżącą sesję. Jesli sesja nie istnije, zwracana jest atrapa
     *
     * @return
     */
    public default BaseSession session() {
        return http().session;
    }

    public default ControllerEndpoint<? extends Controller> endpoint() {
        ControllerEndpoint cData = ControllersMap.get(this);
        if (cData == null)
            throw new CoreException(LServlet.CONTROLLER_REQUEST_NOT_FOUND.toString());
        return cData;
    }

    public default BaseUserData user() {
        return session().user;
    }

    public default RequestParams params() {
        return http().params;
    }

    public abstract void onRequest(HttpRequest http) throws Exception;

    public default boolean onException(Throwable ex, HttpRequest http, TMethod method, int code) {
        return false;
    }

    public static Controller getInstance(Thread thread, boolean mustExists) {
        HttpRequest req = HttpRequest.getInstance(thread);
        if (mustExists && (req == null || req.controller == null))
            throw new RuntimeException(LServlet.THREAD_HANDLER_NOT_FOUND.toString(thread.getId()));

        return req != null ? req.controller : null;
    }

    public static Controller getInstance() {
        return getInstance(true);
    }

    public static Controller getInstance(boolean mustExists) {
        return getInstance(Thread.currentThread(), mustExists);
    }

    public default <C extends Controller> C forward(Class<C> cPage, boolean doRequest) throws Exception {
        C ctrl = cPage.newInstance();
        HRequests.instance().onAfterCreateRequestHandler(ctrl);
        //   page.forwardedFrom = (Class<Page>) this.getClass();
        if (doRequest)
            ctrl.onRequest(http());
        return ctrl;
    }

    default void processOptionsMethod(HttpRequest http) {
        Strings mths = new Strings();
        for (HttpMethod mth : endpoint().methods())
            mths.add(mth.name());
        http.setHeader("Allow", mths.toString(", "));
    }
}

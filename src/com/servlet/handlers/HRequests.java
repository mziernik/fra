package com.servlet.handlers;

import com.context.fra.Framework;
import com.servlet.controller.Page;
import com.servlet.controller.Controller;
import com.utils.reflections.Reflections;
import com.html.bootstrap.BsModalForm;
import com.html.bootstrap.form.BsFormController;
import com.html.core.tag.Tag;
import com.ui.PService;
import com.servlet.Handlers;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.utils.reflections.TClass;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class HRequests {

    // aktywne żądania
    public final static LinkedHashMap<Thread, HttpRequest> requests = new LinkedHashMap<>();  // sync

    // lista oczekujących żądań    
    public final static LinkedList<HttpRequest> requestQueue = new LinkedList<>();  // sync

    public static HRequests instance() {
        return Handlers.requests.getInstance();
    }

    /**
     * Przed wywołaniem konstruktora strony. Tu można dodoać np przekierowania
     */
    public boolean onBeforeCreateRequestHandler(
            Class<? extends Controller> hCls,
            Endpoint ipage, HttpRequest request) throws Exception {
        return true;
    }

    // jeśli metododa zwróci false - przetwarzanie zostanie anulowane
    public boolean onBeforeRequest(Page page) {
        return true;
    }

    /**
     * Metoda wywoływana jest dwukrotnie. Za pierwszym razem current = null
     * Ręczna definicja klasy
     *
     * @param afterResources -
     * @param request
     * @return
     * @throws Exception
     */
    public Class<? extends Controller> getRequestHandler(
            Class<? extends Controller> current,
            HttpRequest request)
            throws Exception {

        if (current == null
                && Framework.isRunning()
                && request.path.length == 1
                && "".equals(request.path[0]))
            return PService.class;

        return current;
    }

    public <T extends Controller> T getController(
            Class<? extends T> cls, HttpRequest request)
            throws Exception {
        return new TClass<>(cls).newInstance(null, request);

    }

    public void onAfterCreateRequestHandler(Controller handler) {

    }

    /**
     * Tu można podpiąć serwlet
     *
     * @param request
     * @param response
     * @return
     */
    public boolean onProcessRequest(HttpServletRequest request, HttpServletResponse response) {
        return false;
    }

    /**
     * Metoda wywoływana jest przed dodaniem klasy do indeksu
     *
     * @param cls
     * @param internal
     * @throws Exception
     */
    public void addPage(Class<? extends Controller> cls, boolean internal) throws Exception {

        TClass<? extends Controller> clazz = new TClass<>(cls);
        if (clazz.instanceOf(BsModalForm.class)) {
            // Muszą być zadeklarowane 2 konstruktory
            Reflections.checkConstructor(cls, HttpRequest.class);
            Reflections.checkConstructor(cls, HttpRequest.class);
        } else if (clazz.instanceOf(BsFormController.class)) {
            // Muszą być zadeklarowane 2 konstruktory
            Reflections.checkConstructor(cls, HttpRequest.class);
            Reflections.checkConstructor(cls, Tag.class);
        }

    }
}

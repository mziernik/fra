package com.servlet.controller;

import com.config.CHttp;
import com.utils.Utils;
import com.utils.Is;
import com.config.CService;
import com.exceptions.CoreException;
import com.exceptions.EError;
import com.exceptions.http.Http400BadRequestParamException;
import com.exceptions.http.Http405MethodNotAllowed;
import com.html.core.Html;
import com.json.JArray;
import com.json.JSON;
import com.lang.LServlet;
import com.servlet.controller.intf.*;
import com.servlet.interfaces.Endpoint;
import com.servlet.interfaces.HttpMethod;
import com.servlet.requests.HttpRequest;
import com.user.right.UserRight;
import com.servlet.views.ViewController;
import java.lang.annotation.Annotation;
import java.util.LinkedList;
import com.servlet.handlers.HRequests;
import com.servlet.interfaces.Arg;
import com.servlet.interfaces.JsMethod;
import com.servlet.requests.ServletInputStreamEx;
import com.thread.ThreadObject;
import com.user.BaseAuthHandler;
import com.user.pages.PLogin;
import com.utils.reflections.TField;
import com.utils.reflections.TMethod;
import java.lang.reflect.Field;
import java.net.URI;
import java.util.Map;

/**
 * @author Miłosz Ziernik
 * @date 07 października 2015
 * @encoding UTF-8
 */
public class ControllerEndpoint<Ctrl extends Controller> implements Endpoint {

    private Controller controller;
    public final HttpRequest http;

    public final ControllerMetaData index;
    public final Thread thread;

    //-------------------------- zdarzenia -----------------------
    public final LinkedList<BeforeRequest> beforeProcessRequest = new LinkedList<>();
    public final LinkedList<AfterRequest> afterProcessRequest = new LinkedList<>();
    public final LinkedList<ControllerEvent> createHtml = new LinkedList<>();
    public final LinkedList<ControllerEvent> onMultipartRequest = new LinkedList<>();
    public final LinkedList<BeforeReturnHtml> beforeReturnHtml = new LinkedList<>();
    public final LinkedList<BeforeReturnContent> beforeReturnContent = new LinkedList<>();
    public ControllerInvoker invoker;

// ----------------------------------------------------
    ControllerEndpoint(ControllerMetaData index, HttpRequest http) {
        this.index = index;
        this.http = http;
        this.thread = Thread.currentThread();

        Endpoint endpoint = def = index.endpoint;

        url = index.hasEndpoint ? endpoint.url() : new String[0];
        title = index.hasEndpoint ? endpoint.title() : "";
        description = endpoint.description();
        auth = endpoint.auth();
        session = endpoint.session();
        logRequests = endpoint.logRequests();
        methods = endpoint.methods();
        disableLogs = endpoint.disableLogs();
        ssl = endpoint.ssl();
        debugOnly = endpoint.devOnly();
        resources = endpoint.resources();
        rights = endpoint.rights();
        view = index.hasEndpoint ? endpoint.view() : ViewController.class;
    }

    public Controller getController() {
        if (controller == null)
            throw new CoreException(LServlet.CONTROLLED_NOT_INITIALIZED_YET.toString(index.controller.getName()));

        return controller;
    }

    public void updateController(Ctrl controller) {
        this.controller = controller;
        this.http.controller = controller;
        if (controller instanceof ControllerInvoker)
            invoker = (ControllerInvoker) controller;
    }

    public void processRequest(EndpointMapping target) throws Exception {
        TMethod method = null;
        try {

            ThreadObject.language.set(http.session.language.get());

            if (!CHttp.url.isEmpty())
                CHttp.url.setDefaultValue(http.getAbsolutePath("", null));

            if (!isMethodAllowed(http.getMethod(), methods()))
                throw new Http405MethodNotAllowed(http);

            if (!BaseAuthHandler.newInstance()
                    .checkPageAuthorization(getController(), false))
                return;

            http.session.newRequest(this, http);

            //------------------
            http.setLogsEnabled(logRequests()
                    && http.getHeader("X-Requested-Skip-Log") == null);

            if (http.isCorsRequest || controller instanceof PLogin)
                http.addCorsHeaders();

            synchronized (http.session.requestCount) {
                http.session.requestCount.incrementAndGet();
            }

            if ("NOP".equals(http.getHeader("ajax-keep-alive"))) {
                //  onKeepAlive();
                http.returnPlainText("keep-alive");
                return;
            }

            for (ControllerEvent event : createHtml)
                event.call(this);

            //-------------------------------------------------------------
            String jsMethodName = http.getHeader("js-method");

            if (jsMethodName != null)
                try (ServletInputStreamEx in = http.getInputStream()) {
                    JArray arr = JSON.parse(in).asArray();
                    for (JsMethod.JsMethodImpl jsMethod : index.jsMethods)
                        if (jsMethod.name.equals(jsMethodName)) {
                            jsMethod.method.invoke(controller, arr);
                            return;
                        }
                }

            method = target != null && target.method != null
                    ? target.method : index.getEndpointMethod(http);

            Endpoint endpoint = method != null ? method.getAnnotation(Endpoint.class)
                    : null;

            if (method != null && endpoint != null)
                if (!http.session.user.rights.checkMethodRights(controller, method.raw, endpoint))
                    return;

            for (BeforeRequest event : beforeProcessRequest)
                event.call(this, method);

            Throwable err = null;

            try {

                /* if (http.multiPart)
                 onUpload(false);

                 if (ajax.isAjaxUpload)
                 onUpload(true);
                 */
                if (http.multiPart)
                    for (ControllerEvent event : onMultipartRequest)
                        event.call(this);

                if (http.response.isCommitted())
                    return;

                if (target != null && target.method != null)
                    if (invoker != null)
                        invoker.invoke(target.lambdaController, http, target.method);
                    else
                        target.method.invoke(target.lambdaController, http);
                else if (method != null) {

                    Object[] arr = method.createArguments((String arg) -> {
                        return http.params.getValuesStr(arg);
                    }, http.relativePath, null);

                    if (invoker != null)
                        invoker.invoke(controller, http, method, arr);
                    else
                        method.invoke(controller, arr);
                } else {
                    for (Map.Entry<Field, Arg.ArgMeta> en : controller.endpoint().index.arguments.entrySet()) {
                        Arg.ArgMeta arg = en.getValue();
                        if (arg.ann == null)
                            continue;

                        LinkedList<String> list = controller.http().params.getList(arg.name);
                        Object obj = null;
                        try {
                            obj = new TField(en.getKey()).set(controller, list);
                        } catch (Throwable e) {
                            throw new Http400BadRequestParamException(http, arg.name, e);
                        }
                        if (obj == null && arg.required)
                            throw new Http400BadRequestParamException(http, arg.name);

                    }
                    if (invoker != null)
                        invoker.invoke(controller, http, null);
                    else
                        controller.onRequest(http);
                }

            } catch (Throwable e) {
                err = e;
                throw e;
            } finally {
                for (AfterRequest event : afterProcessRequest)
                    event.call(this, method, err);
            }

            Html html;
            if (!http.isLocked()
                    && (html = (Html) http.properties.get("$htmlBuilder$")) != null)
                http.returnHTML(html, 200);

        } catch (Throwable ex) {
            int code = EError.getHttpStatus(ex);
            if (!http.isLocked()) {
                http.setStatus(code);
                http.addExceptionHeader(ex);
            }
            if (controller.onException(ex, http, method, code))
                return;
            if (ex instanceof Exception)
                throw (Exception) ex;
            if (ex instanceof Error)
                throw (Error) ex;
            throw new Exception(ex);

        } finally {

            if (http.session != null && http.outputStream != null)
                http.session.bytesReturned += http.outputStream.length;
            if (http.session != null && http.inputStream != null)
                http.session.bytesReceived += http.inputStream.length;

            synchronized (HRequests.requestQueue) {
                // jeśli są połączenia oczekujące to usuń z listy najstarsze
                if (!HRequests.requestQueue.isEmpty())
                    HRequests.requestQueue.pollFirst();
            }

            if (!session() && http.session.isNew())
                http.session.invalidate();

        }
    }

    private static boolean isMethodAllowed(HttpMethod method, HttpMethod[] methods) {
        if (methods == null || methods.length == 0)
            return true;

        if (method != null)
            for (HttpMethod m : methods)
                if (method == m)
                    return true;

        return false;
    }

    //==========================================================================
    private final Endpoint def;
    private final String[] url;
    private String title;
    private String description;
    private boolean auth;
    private boolean session;
    private boolean logRequests;
    private HttpMethod[] methods;
    private boolean disableLogs;
    private boolean ssl;
    private boolean debugOnly;
    private String[] resources;
    private Class<? extends ViewController> view;
    private Class<? extends UserRight>[] rights;

    @Override
    public String[] url() {
        return url;
    }

    public ControllerEndpoint title(String title) {
        this.title = Utils.coalesce(title, def.title());
        return this;
    }

    @Override
    public String title() {
        return title;
    }

    public ControllerEndpoint description(String description) {
        this.description = Utils.coalesce(description, def.description());
        return this;
    }

    @Override
    public String description() {
        return description;
    }

    public ControllerEndpoint auth(Boolean auth) {
        this.auth = Utils.coalesce(auth, def.auth());
        return this;
    }

    @Override
    public boolean auth() {
        return auth;
    }

    public ControllerEndpoint session(Boolean session) {
        this.session = Utils.coalesce(session, def.session());
        return this;
    }

    @Override
    public boolean session() {
        return session;
    }

    @Override
    public Class<? extends ViewController> view() {
        return view;
    }

    public ControllerEndpoint view(Class<? extends ViewController> view) {
        this.view = view != null ? view : ViewController.class;
        return this;
    }

    public ControllerEndpoint logRequests(Boolean logRequests) {
        this.logRequests = Utils.coalesce(logRequests, def.logRequests());
        return this;
    }

    @Override
    public boolean logRequests() {
        return logRequests;
    }

    public ControllerEndpoint methods(HttpMethod[] methods) {
        this.methods = Utils.coalesce(methods, def.methods());
        return this;
    }

    @Override
    public HttpMethod[] methods() {
        return methods;
    }

    public ControllerEndpoint disableLogs(Boolean disableLogs) {
        this.disableLogs = Utils.coalesce(disableLogs, def.disableLogs());
        return this;
    }

    @Override
    public boolean disableLogs() {
        return disableLogs;
    }

    public ControllerEndpoint ssl(Boolean ssl) {
        this.ssl = Utils.coalesce(ssl, def.ssl());
        return this;
    }

    @Override
    public boolean ssl() {
        return ssl;
    }

    public ControllerEndpoint debugOnly(Boolean debugOnly) {
        this.debugOnly = Utils.coalesce(debugOnly, def.devOnly());
        return this;
    }

    @Override
    public Class<? extends UserRight>[] rights() {
        return rights;
    }

    public ControllerEndpoint rights(Class<? extends UserRight>... rights) {
        this.rights = rights;
        return this;
    }

    @Override
    public boolean devOnly() {
        return debugOnly;
    }

    public ControllerEndpoint resources(String[] resources) {
        this.resources = Utils.coalesce(resources, def.resources());
        return this;
    }

    @Override
    public String[] resources() {
        return resources;
    }

    @Override
    public Class<? extends Annotation> annotationType() {
        return Endpoint.class;
    }

}

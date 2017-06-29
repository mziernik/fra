package com.servlet;

import com.config.CHttp;
import com.utils.Utils;
import com.utils.console.TConsole;
import com.servlet.page.PTechnicalBreak;
import com.servlet.controller.Controller;
import com.config.CService;
import com.exceptions.http.Http503ServiceUnavailable;
import com.exceptions.http.PageNotFound;
import com.mlogger.Log;
import com.servlet.controller.ControllerMetaData;
import com.servlet.requests.HttpRequest;
import com.context.AppContext;
import com.context.*;
import com.dev.console.DevConsole;
import com.exceptions.*;
import com.lang.LServlet;
import com.service.status.StatusGroup;
import com.servlet.controller.*;
import com.servlet.handlers.*;
import com.servlet.handlers.temporary.*;
import com.servlet.interfaces.*;
import com.servlet.webservices.WebServices;
import com.servlet.webservices.WsInitializer;
import com.thread.ThreadObject;
import com.utils.*;
import com.utils.collections.Strings;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.*;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.*;

@WebServlet(name = "Main Servlet", urlPatterns = {"/"}, asyncSupported = true, loadOnStartup = 0)
@MultipartConfig(fileSizeThreshold = 1024 * 1024)
public class MainServlet extends HttpServlet implements Filter {

    public final static StatusGroup STATUS = StatusGroup.SERVICE.group("http", "Żądania HTTP");

    private static MainServlet instance;

    private static void handleRequest(HttpRequest http) {

        Thread.currentThread().setPriority(Thread.NORM_PRIORITY);

        String oryginalThreadName = Thread.currentThread().getName();
        Thread.currentThread().setName("Request, " + http.request.getMethod()
                + ", " + http.request.getServletPath()
                + (http.request.getQueryString() != null
                ? "?" + http.request.getQueryString() : ""));

        Class<? extends Controller> hCls = null;
        EndpointMapping target = null;//
        Controller controller = null;

        try {
            AppContext.waitForContextInitialized();

            checkLimits(http);

            // metody trybu deweloperskiego
            if (http.relativePath.equals("$dev")) {
                DevConsole.handle(http);
                return;
            }

            if (AppContextInitializer.hasInitErrors()) {
                Handlers.errors.getInstance().onException(
                        http, AppContextInitializer.getInitError());
                return;
            }

            if (new ProxyForwarder(http).forward())
                return;

            if (!CService.CTechnicalBreak.checkAccess(
                    InetSocketAddress.createUnresolved(http.request.getRemoteAddr(),
                            http.request.getRemotePort())))
                hCls = PTechnicalBreak.class;

            hCls = HRequests.instance().getRequestHandler(hCls, http);

            // ------------------------ kontroler ----------------------
            if (hCls == null) {

                LinkedList<EndpointMapping> mappings = EndpointMapping.get(http.relativePath);

                if (mappings.size() > 1) {
                    Strings matches = new Strings();

                    for (EndpointMapping cm : mappings)
                        matches.add(cm.meta.controller.getName() + " \"" + cm.url + "\"");

                    if (matches.size() > 1)
                        Log.warning("Żądanie \"" + http.relativePath
                                + "\" pasuje do wielu klas: \n" + matches.toString("\n"));
                }

                target = mappings.peek();
                if (target != null) {
                    hCls = target.meta.controller;
                    http.endpointMapping = target;
                }
            }

            // ------------- jeśli żądanie dotyczy pliku zasobu ------------
            if (hCls == null)
                Handlers.resources.getInstance().processRequest(http);

            if (http.response.isCommitted())
                return;

            if (hCls == null)
                hCls = TempRequest.processRequest(http);

            if (http.outputStream != null)
                return;

            if (hCls == null)
                hCls = http.session.forcedPage;

            hCls = HRequests.instance().getRequestHandler(hCls, http);

            // jeśli url kończy się na  "/" i nie znaleziono kontrolera, to wykonaj przekierowanie na andres nie zawierający "/"
            if (hCls == null && http.relativePath.length() > 1 && http.relativePath.endsWith("/")) {
                http.redirect(http.toAbsoluteUrl(false));
                return;
            }

            if (hCls == null)
                throw new PageNotFound(http);

            Endpoint ipage = hCls.getAnnotation(Endpoint.class);

            if (!HRequests.instance().onBeforeCreateRequestHandler(hCls, ipage, http))
                return;

            // jesli żądanie przyszło z tej samej domeny ale z innego protokołu (np http -> https)
            // to dodaj nagłówek zwzwalający na połączenia CORS (XSS) ajaxa
//            if (ipage != null && !Is.empty(http.getHeader("referer"))) {
//                Url referer = new Url(http.getHeader("referer"));
//                if (Objects.equals(referer.host(), http.url.host())
//                        && !Objects.equals(referer.protocol(), http.url.protocol())) {
//                    String allow = referer.toString();
//                    if (allow.indexOf("/", allow.indexOf("://") + 3) > 0)
//                        allow = allow.substring(0, allow.indexOf("/", allow.indexOf("://") + 3));
//                    http.setHeader("Access-Control-Allow-Origin", allow);
//                    http.setHeader("Access-Control-Allow-Credentials", "true");
//
//                    Log.debug("HTTP", "Połączenie z innego protokołu do tej samej domeny.\n"
//                            + "Dodaję nagłówek zezwalający na wykonanie żądania z " + allow + " (CORS)");
//                }
//            }
            // przekieruj na SSL-a jeśli jest dostępny, oraz strona tego wymaga
            if (ipage != null && ipage.ssl() && !http.request.isSecure()) {
                if (http.isCorsRequest)
                    http.addCorsHeaders();

                String current = http.url.toString();
                if (current.contains("?"))
                    current = current.substring(0, current.indexOf("?"));
                if (current.contains("#"))
                    current = current.substring(0, current.indexOf("#"));

                Url url = http.getAbsolutePath(http.url.toString(), true);

                String redir = url.toString();
                if (redir.contains("?"))
                    redir = redir.substring(0, redir.indexOf("?"));
                if (redir.contains("#"))
                    redir = redir.substring(0, redir.indexOf("#"));

                if (redir.startsWith("https://") && !current.equals(redir)) {
                    http.redirect(url);
                    return;
                }
            }

            ControllerMetaData meta = ControllerMetaData.getByClass(hCls);
            if (meta == null)
                throw new CoreException(LServlet.CONTROLLER_DEF_NOT_FOUND.toString(
                        hCls.getName()
                ));

            ControllerEndpoint<?> ctrlImpl = meta.endpoint(http);

            ControllersMap.add(http, ctrlImpl);

            controller = HRequests.instance().getController(hCls, http);
            ControllersMap.add(controller, ctrlImpl);

            HRequests.instance().onAfterCreateRequestHandler(controller);

            if (http.method.equals(HttpMethod.OPTIONS)) {
                http.addCorsHeaders();
                controller.processOptionsMethod(http);
                http.getOutputStream().close();
                return;
            }

            http.logRequest();
            controller.endpoint().processRequest(target);
        } catch (Throwable ex) {
            http.logRequest();
            int code = EError.getHttpStatus(ex);
            if (!http.isLocked()) {
                http.setStatus(code);
                http.addExceptionHeader(ex);
            }
            if (code >= 200 && code <= 499)
                Log.warning(ex);
            else
                Log.error(ex);
            if (!http.isLocked()) {
                http.setStatus(code);
                Handlers.errors.getInstance().onException(http, ex);
            } else
                TConsole.printErr(ex);
        } finally {
            try {
                http.logRequest();
                http.logResponse();
            } catch (Throwable e) {
                Log.error(e);
            }

            if (controller != null)
                ControllersMap.remove(http, controller);
            Thread.currentThread().setName(oryginalThreadName);
            ThreadObject.clear();
        }
    }

    public static class ControllersMap {

        private final static Object sync = new Object();
        private final static Map<HttpRequest, ControllerEndpoint> classes = new HashMap<>();
        private final static Map<Controller, ControllerEndpoint> instances = new HashMap<>();

        public static ControllerEndpoint get(Controller controller) {
            synchronized (sync) {
                ControllerEndpoint cdata = instances.get(controller);

                if (cdata == null) {
                    cdata = classes.get(HttpRequest.getInstance());
                    if (cdata != null)
                        instances.put(controller, cdata);
                }

                return cdata;
            }
        }

        static void add(HttpRequest http, ControllerEndpoint ctrlImpl) {
            synchronized (sync) {
                classes.put(http, ctrlImpl);
            }
        }

        static void add(Controller controller, ControllerEndpoint ctrlImpl) {
            ctrlImpl.updateController(controller);
            synchronized (sync) {
                instances.put(controller, ctrlImpl);
            }
        }

        static void remove(HttpRequest http, Controller controller) {
            synchronized (sync) {
                instances.remove(controller);
                classes.remove(http);
            }
        }
    }

    private static void checkLimits(HttpRequest request)
            throws Http503ServiceUnavailable, InterruptedException {

        if (HRequests.requests.size() > CHttp.maxRequestCount.value()
                && HRequests.requestQueue.size() > CHttp.maxRequestQueueSize.value()) {
            Log.warning("Context", "Przekroczony limit połączeń ("
                    + CHttp.maxRequestCount.value() + "), oraz rozmiar kolejki ("
                    + CHttp.maxRequestQueueSize.value() + ")");

            // przekroczony limit żądań oraz rozmiar kolejki
            throw new Http503ServiceUnavailable(null, LServlet.CONNECTION_LIMIT_EXCEEDED.toString());

        }

        if (HRequests.requests.size() > CHttp.maxRequestCount.value()) {
            // przekroczony limit żądań, można kolejkować
            synchronized (HRequests.requestQueue) {
                HRequests.requestQueue.add(request);
            }
            Log.warning("Context", "Przekroczony limit połączeń ("
                    + CHttp.maxRequestCount.value() + "), dodaje żądanie do kolejki ");

            boolean busy = true;
            while (busy) {
                // czekaj, dopuki żądanie jest na liście oczekujących
                Thread.sleep(100);
                synchronized (HRequests.requestQueue) {
                    busy = HRequests.requestQueue.indexOf(request) >= 0;
                }
            }
        }
    }

    public static class Param {

        public String name;
        public String value;

        public Param(String name, String value) {
            this.name = name;
            this.value = value;
        }
    }

    public final static ThreadPoolExecutor threadPool
            = new ThreadPoolExecutor(5, 300, 500000L, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<>());

    @Override
    public void init() throws ServletException {
        super.init();
        instance = this;
        Log.event("servlet", "MainServlet initialization");
        try {
            // Wyłączenie tworzenia ciastek JSESSIONID
            ServletContext ctx = getServletContext();
            if (ctx.getSessionCookieConfig() != null)
                ctx.setSessionTrackingModes(Collections.EMPTY_SET);
        } catch (Exception e) {
            TConsole.printErr(e);
        }
    }

    /**
     *
     * @param fc
     * @throws ServletException
     */
    @Override
    public void init(FilterConfig fc) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest sr, ServletResponse sr1, FilterChain filterChain)
            throws IOException, ServletException {
        try {
            Handlers.requestFilter.getInstance().doFilter(sr, sr1, filterChain);
        } catch (Throwable e) {
            TConsole.printErr(e);
            throw e;
        }
    }

    @Override
    public void destroy() {
        threadPool.shutdown();
        super.destroy();
    }

    public static void processAsyncRequest(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {

        String eTag = req.getHeader("if-none-match");
        if (eTag != null && Handlers.resources.getInstance().processETag(req, resp, eTag))
            return;

        if (TransparentProxy.process(req, resp))
            return;

        final AsyncContext asyncContext = req.startAsync(req, resp);

        asyncContext.setTimeout(24 * 60 * 60 * 1000);
        threadPool.execute(() -> {
            HttpServletRequest request = (HttpServletRequest) asyncContext.getRequest();
            HttpServletResponse response = (HttpServletResponse) asyncContext.getResponse();
            try {
                String servletPath = Utils.coalesce(request.getServletPath(), "");

                if (servletPath.startsWith("/"))
                    servletPath = servletPath.substring(1);

                if (servletPath.endsWith("/"))
                    servletPath = servletPath.substring(0, servletPath.length() - 1);

                if (WsInitializer.hasLibrary
                        && WebServices.processRequest(servletPath, request, response))
                    return;

                if (Handlers.requests.getInstance().onProcessRequest(request, response))
                    return;

                if (!checkContextInitialized(request, response))
                    return;

                Thread thread = Thread.currentThread();
                final HttpRequest hReq = new HttpRequest(request, response);
                try {
                    STATUS.itemStr(hReq.id, hReq.id + ", " + hReq.url);

                    if (hReq.isLocked())
                        return;

                    HttpRequest rw = HttpRequest.getInstance();
                    if (rw != null && rw.processed)
                        throw new RuntimeException(LServlet.CURRENT_THREAD_BUSY.toString(
                                rw.thread.getName()));

                    synchronized (HRequests.requests) {
                        HRequests.requests.put(thread, hReq);
                    }

                    hReq.processed = true;

                    handleRequest(hReq);

                } finally {
                    STATUS.remove(hReq.id);
                    synchronized (HRequests.requests) {
                        HRequests.requests.remove(thread);
                    }
                    hReq.processed = true;
                }

            } catch (Throwable e) {
                printError(e, response);
            } finally {
                try {
                    asyncContext.complete();
                } catch (Throwable e) {
                    // Może pojawić się wyjątek java.lang.IllegalStateException:
                    // The request associated with the AsyncContext has already completed processing.
                    // Można to zignorować
                }
            }
        });
    }

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processAsyncRequest(request, response);
    }

    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processAsyncRequest(request, response);
    }

    @Override
    public void service(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processAsyncRequest(request, response);
    }

    @Override
    public void doOptions(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processAsyncRequest(request, response);
    }

    @Override
    public void doPut(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processAsyncRequest(request, response);
    }

    @Override
    protected void doTrace(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processAsyncRequest(request, response);
    }

    /**
     * Przekieruj dane żądanie do innego servletu
     *
     * @param servlet
     * @param req
     * @param resp
     */
    public static void redirect(HttpServlet servlet, HttpServletRequest req, HttpServletResponse resp) throws Exception {
        String m = req.getMethod().toLowerCase();
        m = m.substring(0, 1).toUpperCase() + m.substring(1);
        Method method = servlet.getClass().getDeclaredMethod("do" + m,
                HttpServletRequest.class, HttpServletResponse.class);
        method.setAccessible(true);
        method.invoke(servlet, req, resp);
    }

    // lista adresow klientow, z ktorych wywolane byly żądania przed zainicjalizowaniem kontekstu
    private final static Set<String> checkContextInitCalls = new LinkedHashSet<>();

    private static boolean checkContextInitialized(HttpServletRequest req, HttpServletResponse resp) {

        try {
            if (!AppContext.isInitialized()) {
                String remoteHost = req.getRemoteHost();

                if (checkContextInitCalls.contains(remoteHost)) {
                    AppContext.waitForContextInitialized();
                    return true;
                }

                checkContextInitCalls.add(remoteHost);

                resp.setStatus(503);
                resp.setContentType("text/html; charset=UTF-8");
                resp.setCharacterEncoding("UTF-8");

                try (PrintWriter writer = resp.getWriter()) {
                    writer.append("<body style=\"font-family: Verdana, DejaVu Sans, Tahoma;\">\n")
                            .append("  <center>\n")
                            .append("    <br/><br/>\n")
                            .append("    <h5>Proszę czekać</h5>\n")
                            .append("    <h3>Trwa inicjalizacja usługi</h3>\n")
                            .append("    <h6>Strona odświeży się automatycznie</h6>\n")
                            .append("  </center>\n")
                            .append("</body>\n")
                            .append("<script type=\"text/javascript\">")
                            .append("  location.reload();\n")
                            .append("</script>\n")
                            .append("</html>")
                            .flush();
                }
                return false;
            }

        } catch (Throwable e) {
            Log.error(e);
        }

        return true;
    }

    public static MainServlet getInstance() {
        new WaitFor(LServlet.INIT_SERVLET.toString(),
                () -> instance != null);
        return instance;
    }

    static void printError(Throwable e, HttpServletResponse response) {
        Logger.getLogger("").log(Level.SEVERE, null, e);
        try {
            response.setContentType("text/plain; charset=UTF-8");
            response.setStatus(500);
            try (PrintWriter writer = response.getWriter();) {
                e.printStackTrace(writer);
            }
        } catch (Exception ex) {
        }
    }
}

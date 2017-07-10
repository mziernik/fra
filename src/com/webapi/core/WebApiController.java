package com.webapi.core;

import com.cache.CachedData;
import com.config.CHttp;
import com.thread.ThreadObject;
import com.config.CService;
import com.context.AppContext;
import com.exceptions.EError;
import com.exceptions.ErrorMessage;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;
import com.json.JSON;
import com.json.JValue;
import com.lang.core.Language;
import com.lang.core.Languages;
import com.mlogger.*;
import com.model.repository.Repository;
import com.servlet.MainServlet.ControllersMap;
import com.servlet.controller.BaseSession;
import com.servlet.controller.Controller;
import com.servlet.interfaces.HttpMethod;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.ServletInputStreamEx;
import com.servlet.websocket.WebSocketConnection;
import com.servlet.websocket.WebSocketController;
import com.thread.ThreadDump;
import com.utils.*;
import com.utils.collections.*;
import com.utils.collections.Params.Param;
import com.utils.date.TDate;
import com.utils.hashes.Hashes;
import com.utils.reflections.TClass;
import com.webapi.WService;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.*;

//ToDo: Dodać obłsugę flagi Deprecated
public abstract class WebApiController extends WebSocketController
        implements Controller, WebApi {

    public final Set<Repository> repositories = new HashSet<Repository>();

    protected ExecutorService getExecutor() {
        if (threadPool == null)
            threadPool = new ThreadPoolExecutor(100, 300, 500000L, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<>());
        return threadPool;
    }

    public Invoker invoker;
    private static ThreadPoolExecutor threadPool;

    final SyncMap<String, WebApiRequest> requests = new SyncMap<>();

    final static SyncMap<Class<? extends WebApiController>, String> hashes = new SyncMap<>();
    public final BaseSession session;
    private final ThreadDump dump;

    public WebApiController() {
        Language lang = language.get();

        BaseSession ses = connection != null ? connection.httpSession
                : Is.notNullR(ControllersMap.get(this), (endp) -> endp.http.session);

        this.session = Is.nullR(ses, () -> connection != null
                ? new BaseSession(connection) : new BaseSession()
        );

        lang = this.session.language.get();

        language.set(lang);
        ThreadObject.language.set(lang);
        dump = new ThreadDump();
    }

    @WebApiEndpoint
    public final WService service = new WService();

    protected boolean canSendEvent(String source, String event, Object data) {
        return true;
        //FiMe
        /*
        String hash = Utils.coalesce(service.notifications.currentHash.last(), "");

        if (!WNotifications.notifySources.containsKey(source))
            throw new ServiceException("Unknown notify source: " + source);

        for (Pattern p : service.notifications.notifyPatterns.get(source))
            if (p.matcher(hash).matches())
                return true;

        return false;*/
    }

    @Override
    public void onMessage(String message) {

        // Każda funkcja wywoływana jest w innnym wątku
        ThreadObject.parentThread.set(dump);

        WebApiRequest req = null;
        Thread th = Thread.currentThread();
        String requestId = null;
        try {
            JObject data = JObject.parse(message);
            requestId = data.getStr("id");

            String event = data.getStr("event", "");

            if (!event.isEmpty()) {

                req = requests.get(requestId);

                if ("cancel".equals(event)) {
                    req.cancelled = true;
                    return;
                }

                if (req != null && req.onEvent != null)
                    req.onEvent.run(event, data.element("data", null));
                else
                    Log.warning("Method 'onEvent' not implemented");
                return;
            }

            Url url = new Url(connection.requestUrl);
            if ("ws".equalsIgnoreCase(url.protocol()))
                url.protocol("http");
            if ("wss".equalsIgnoreCase(url.protocol()))
                url.protocol("https");

            req = new WebApiRequest(this, null, this,
                    Utils.coalesce(Languages.get(data.getStr("lang", null)), language.get()),
                    url,
                    data.getStr("location", null),
                    data.getStr("endpoint"),
                    requestId,
                    data.objectD("params"));

            req.source = data;
            th.setName("WebApi: " + req.endpointName);

            for (JValue val : data.objectD("headers").getValues())
                req.headers.add(val.getName(), val.asString());

            process(req);

        } catch (Throwable ex) {
            try {
                response(connection, null, req, requestId, null, ex);
            } catch (Throwable e) {
                Log.error(e);
            }
        } finally {

        }

    }

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        String requestId = null;
        WebApiRequest req = null;
        try {

            int idx = 0;
            for (char c : http.endpointMapping.url.toCharArray())
                if (idx < http.relativePath.length() && http.relativePath.charAt(idx) == c)
                    ++idx;
                else
                    break;

            if (idx == http.relativePath.length()) {
                Html.printInfo(this, http, getClass(), "");
                return;
            }

            Strings spath = new Strings()
                    .nonEmpty(true)
                    .addAll(http.relativePath.substring(idx).split("/"));

            JElement content = null;

            if (http.method == HttpMethod.POST
                    && Is.in(http.contentType.toLowerCase(),
                            "application/json",
                            "application/javascript",
                            "text/json",
                            "text/javascript"
                    ))
                try (ServletInputStreamEx in = http.getInputStream()) {
                    content = JSON.parse(in);
                }

            if (http.method == HttpMethod.POST
                    && http.params.has("%data")
                    && http.contentType.equalsIgnoreCase("application/x-www-form-urlencoded")) {
                content = JSON.parse(http.params.get("%data"));
                http.params.remove("%data");
            }

            //FixMe
            JObject params = new JObject();

            req = new WebApiRequest(this,
                    http,
                    null,
                    Utils.coalesce(Languages.get(http.headers.get("lang")), language.get()),
                    http.url,
                    http.headers.get("location"),
                    spath.toString("/"),
                    null,
                    params);

            req.headers.addAll(http.headers);

            process(req);

        } catch (Throwable ex) {
            response(null, http, req, requestId, null, ex);
        }

    }

    public Pair<WebApiControllerMeta, WebApi> getEndpoint(WebApiRequest req, String spath) throws Exception {

        LinkedList<String> path = new Strings()
                .nonEmpty(true)
                .addAll(spath.split("\\/"))
                .astList();

        WebApi wapi = this;
        WebApiControllerMeta meta = null;

        while (!path.isEmpty()) {
            String s = path.poll();
            meta = null;
            for (WebApiControllerMeta m : WebApiControllerMeta.map.get(wapi.getClass())) {

                meta = null;
                if (s.equals(m.name)) {
                    meta = m;
                    Class<?> ret = m.item.getReturnType();
                    if (new TClass<>(ret).instanceOf(WebApi.class))
                        wapi = (WebApi) m.invoke(req, wapi, false);
                    break;
                }
            }
            if (meta == null || wapi == null)
                return null;
        }

        if (meta == null || wapi == null)
            return null;

        return new Pair<>(meta, wapi);
    }

    private void process(WebApiRequest req) throws Exception {

        HttpRequest http = req.http;
        WebSocketConnection wsConn = req.webSocket != null ? req.webSocket.connection : null;
        logRequest(req, wsConn, http);

        WebApiControllerMeta meta = req.meta;
        WebApi wapi = req.instance;

        if (meta.returnWebApi != null) {
            if (http != null)
                Html.printInfo(this, http, meta.returnWebApi, req.endpointName);
            return;
        }

        if (meta.returnWebApi == null
                && http != null
                && http.params.size() == 1
                && ".tester".equals(http.params.firstName())) {
            Html.tester(this, http, req);
            return;
        }

        ThreadDump dump = new ThreadDump();

        Runnable runnable = () -> {

            Thread th = Thread.currentThread();
            ThreadObject.webApiReq.set(th, req);
            ThreadObject.parentThread.set(dump);
            ThreadObject.language.set(language.get());

            String thName = th.getName();
            th.setName("WebApi: " + req.endpointName);
            try {
                Object result = meta.invoke(req, wapi);

                if (req.isResponseCommited())
                    return;

                if (meta.endp.async())
                    return;

                req.responseCommit(result);

            } catch (Throwable e) {
                response(wsConn, http, req, req.id, null, e);
            } finally {
                th.setName(thName);
                ThreadObject.clear();
            }
        };

        if (http != null || meta.endp.async())
            runnable.run();
        else {
            ExecutorService exec = getExecutor();

            exec.execute(runnable);

            // Future<?> submit = exec.submit(runnable);
            // System.out.println(" ExecutorService: " + submit.isCancelled() + "  " + submit.isDone());
        }
    }

    static JObject buildEvent(WebApiRequest req, String source, String name, Object data) {

        JObject obj = new JObject();
        obj.options.compactMode(true);
        obj.put("type", "event");
        obj.put("date", new TDate().getTime());

        if (req != null)
            // obj.put("endpoint", req.endpointName);
            obj.put("id", req.id);

        obj.put("source", source);
        obj.put("event", name);
        obj.put("data", data);

        return obj;
    }

    void response(WebSocketConnection wsConn, HttpRequest http, WebApiRequest req, String requestId, Object result, Throwable ex) {

        if (ex == null
                && req != null
                && req.source != null
                && result instanceof Repository) {
            JObject jExport = req.source.objectD("params").object(".export");
            if (jExport != null)
                try {
                    jExport.remove();
                    /*   CachedData data = DsUtils.export((AbstractDataSet) result, req, jExport);
                    if (data != null) {
                        response(wsConn, http, req, requestId, data, null);
                        return;
                    }*/
                } catch (Throwable e) {
                    ex = e;
                    result = null;
                }
        }

        if (req != null && req.isResponseCommited())
            return;

        if (req != null)
            requests.remove(req.id);
        JObject obj = new JObject();
        ErrorMessage error = null;
        try {

            boolean compact = http == null || (!AppContext.devMode && CService.releaseMode());

            if (result instanceof Repository)
                result = ((Repository) result).getJson(true, true);

            if (result instanceof Repository[]) {
                JArray arr = new JArray();
//                for (Repository ds : (Repository[]) result)
//                    ds.getJson(arr.object());
                result = arr;
            }

            if (!compact && result instanceof JArray)
                for (JArray arr : ((JArray) result).getArrays())
                    arr.options.singleLine(true);

            obj.options.compactMode(http == null || (!AppContext.devMode && CService.releaseMode()));

            obj.put("type", ex == null ? "response" : "error");
            obj.put("mode", CService.mode.value().name().toLowerCase());
            obj.put("date", new TDate().getTime());
            obj.put("lang", language.get().key);
            //  obj.put("mode", CService.mode.value().name().toLowerCase());

            obj.put("id", requestId);

            String hash = null;
            if (req != null) {
                obj.put("lang", req.language.key);
                obj.put("duration", System.currentTimeMillis() - req.created.getTime());
                obj.put("endpoint", req.endpointName);
                if (req.meta != null)
                    hash = req.meta.hash;

                obj.put("headers", req.responseHeaders);
            }

            obj.put("hash", getHash() + (hash != null ? "/" + hash : ""));

            obj.put("error", ex != null);

            if (ex != null) {
                error = EError.format(ex);
                Strings det = new Strings();

                for (Map.Entry<String, String> en : error.details.entrySet())
                    if (!Is.empty(en.getValue()))
                        det.add((!Is.empty(en.getKey()) ? en.getKey() + ": " : "") + en.getValue());

                if (CService.devMode() || CService.testMode())
                    det.add("Exception:\n" + EError.exceptionToStr(ex, true));

                obj.put("critical", error.critical);

                obj.arrayC("messages")
                        .object()
                        .put("title", error.title)
                        .put("value", error.message)
                        .put("details", det.toString(", "))
                        .put("type", "error");

                if (http != null) {
                    http.setNoChacheHeader();
                    http.setStatus(EError.getHttpStatus(ex));
                    http.returnCustom(obj.toString(), "application/json; charset=UTF-8");
                } else
                    send(obj.toString());

                return;

            }

            if (req != null)
                obj.arrayC("messages").addAll(req.jMessages.getObjects());

            if (result instanceof CachedData) {

                CachedData cd = (CachedData) result;
                JObject jfile = obj.objectC("file");
                jfile.add("id", cd.key);
                jfile.add("url", CHttp.url(req != null ? req.url : null, cd.key, null).toString());
                jfile.add("name", cd.retrunName);
                jfile.add("mimeType", cd.mimeType);
                jfile.add("size", cd.length());
                jfile.add("expire", cd.getLeaveTime() / 1000);
                result = null;
            }

            obj.put("data", result instanceof Void ? null : result);

            if (http != null) {
                if (req != null) {
                    http.contentDisposition.setHeader(
                            StrUtils.formatFileName(req.endpointName.replace("/", "-")) + ".json");
                    http.contentDisposition.inline = true;
                }
                http.returnJson(obj);
            } else
                send(obj.toString());

        } finally {
            logResponse(req, wsConn, http, obj, error
            );
        }

    }

    private void logRequest(WebApiRequest req, WebSocketConnection wsConn, HttpRequest http) {

        Log log = new Log(LogKind.DEBUG)
                .tag("WebApi", "Request")
                .details(req.json != null && !req.json.isNull() ? req.json : null)
                .value(req.endpointName)
                .comment(req.params.toString());

        log.attribute("Protocol", http != null ? "HTTP" : "WebSocket");

        BaseSession ses = req != null ? req.session : http != null ? http.session
                : wsConn.httpSession;

        if (ses != null) {
            log.attribute("SessionID", ses.id);
            log.device(ses.userAgent.toString());
            log.address(ses.remoteAddress.getHostString());
            log.url(ses.url.toString());
            if (ses.user != null)
                log.user(ses.user.username);
        }

        log.data("Params", req.params.toString());

        for (Param p : req.headers)
            log.attribute("Headers", p.name, p.value);

        log.send();
    }

    void logResponse(WebApiRequest req, WebSocketConnection wsConn, HttpRequest http, JObject json, ErrorMessage e) {

        json.options.compactMode(false);

        Log log = new Log(e != null
                ? (e.critical ? LogKind.ERROR : LogKind.WARNING)
                : LogKind.DEBUG)
                .tag("WebApi", "Response")
                .details(json.toString())
                .value((req != null ? req.endpointName : "")
                        + (e != null ? " -> " + e.exception : ""));
        if (req != null)
            log.comment(req.params.toString());

        log.attribute("Protocol", http != null ? "HTTP" : "WebSocket");

        BaseSession ses = req != null ? req.session : http != null ? http.session
                : wsConn != null ? wsConn.httpSession : null;

        if (ses != null) {
            log.attribute("SessionID", ses.id);
            log.device(ses.userAgent.toString());
            log.address(ses.remoteAddress.getHostString());
            log.url(ses.url.toString());
            if (ses.user != null)
                log.user(ses.user.username);
        }

        if (req != null && req.params != null)
            log.data("Params", req.params.toString());

        if (e != null) {

            Throwable ex = e.exception.exception;

            if (!AppContext.releaseMode()
                    && !AppContext.testMode()
                    && req != null
                    && req.json != null
                    && !req.json.isNull())
                EError.addDetails(ex, "Request", req.json.toString());

            log.setErrorStackTrace(ex)
                    .setExceptionDetails(ex)
                    .setException(ex)
                    .setSourceCode(ex);
        }

        if (req != null)
            for (Entry<String, Object> en : req.responseHeaders.entrySet())
                log.attribute("Headers", en.getKey(), en.getValue());

        log.send();
    }

    private String hash = null;

    public String getHash() {
        if (hash == null) {
            StringBuilder sb = new StringBuilder();
            getHash(getClass(), sb);
            hash = Hashes.idHash6(sb.toString());
        }
        return hash;
    }

    static void getHash(Class<? extends WebApi> cls, StringBuilder sb) {

        TList<WebApiControllerMeta> list = WebApiControllerMeta.map.get(cls);

        list.sort((WebApiControllerMeta o1, WebApiControllerMeta o2) -> o1.name.compareTo(o2.name));

        for (WebApiControllerMeta m : list)
            if (m.returnWebApi != null)
                getHash(m.returnWebApi, sb);

        for (WebApiControllerMeta m : list)
            if (m.returnWebApi == null)
                sb.append(m.hash);
    }

    public static void broadcast(String source, String event, Object data) {
        broadcast(source, event, data, null);
    }

    public static void broadcast(String source, String event, Object data, Collection<WebApiController> recipients) {

        if (recipients == null)
            recipients = WebSocketConnection.getControllers(WebApiController.class);

        if (recipients.isEmpty())
            return;

        String str = buildEvent(null, source, event, data).toString();

        new Log(LogKind.TRACE)
                .tag("WebApi|Broadcast")
                .value(source + ", " + event)
                .details(data)
                .send();

        for (WebSocketController ctrl : recipients)
            ctrl.send(str);

    }
}

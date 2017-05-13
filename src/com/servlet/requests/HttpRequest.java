package com.servlet.requests;

import com.utils.StrUtils;
import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import com.servlet.controller.Controller;
import com.cache.CachedData;
import com.io.IOUtils;
import com.servlet.UserAgent;
import com.config.CContent;
import com.config.CHttp;
import com.context.AppContext;
import com.dev.Dev;
import com.exceptions.EError;
import com.exceptions.ErrorMessage;
import com.exceptions.ThrowableException;
import com.exceptions.http.Http405MethodNotAllowed;
import com.html.core.Html;
import com.html.core.tag.Element;
import com.html.js.JsActions;
import com.html.js.core.JsAction;
import com.io.SearchFiles;
import com.json.*;
import com.lang.LServlet;
import com.lang.core.Language;
import com.lang.core.Languages;
import com.mlogger.ServletLog;
import com.servlet.controller.BaseSession;
import com.utils.collections.Params;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import java.io.*;
import java.net.URLDecoder;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.resources.Res;
import com.servers.WebAppServer;
import com.servlet.*;
import com.servlet.controller.ControllerEndpoint;
import com.servlet.controller.EndpointMapping;
import com.servlet.controller.intf.BeforeReturnHtml;
import com.servlet.handlers.*;
import com.servlet.interfaces.HttpMethod;
import com.utils.Str;
import com.utils.Url;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.xml.XML;
import java.util.Map.Entry;
import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.Part;
import com.servlet.controller.intf.BeforeReturnContent;
import java.nio.charset.Charset;

public class HttpRequest {

    public final HttpServletRequest request;
    public final HttpServletResponse response;
    public final String relativePath; // ścieżka relatywna (bez kontekstu)
    public final String contextPath; // ścieżka kontekstu, najczęściej "/" (dla ROOT-a)
    public Controller controller;
    public final Thread thread = Thread.currentThread();
    public ByteArrayOutputStream inputData = null; // fragment danych wejściowych (jeśli odebrano)
    public ServletInputStreamEx inputStream = null;
    public final UserAgent userAgent;
    public final BaseSession session;
    public final Url url; // pelny adres żądania wraz z parametrami
    public final RequestParams params = new RequestParams(this);
    public final RequestParams queryParams = new RequestParams(this);
    public final Params headers = new Params();
    public final LinkedHashMap<String, Object> properties = new LinkedHashMap<>(); // parametry ogolnego zastosowania
    public boolean processed; // czy żądanie jest aktualnie przetwarzane
    public final String proxyName; // nazwa serwera proxy pobrana z nagłówka żądania
    public final String parentRequestId;
    public final String id = Utils.randomId(15);
    public final Map<String, Object> data = new HashMap<>(); // dowolne dane użytkownika
    public final boolean multiPart;
    public final long createTS = System.nanoTime();
    public final TDate createTime = new TDate();
    public final HttpMethod method;
    public String parametersEncoding = "UTF-8";
    public final String contentType;
    public final boolean isAjaxRequest;
    public final boolean isCorsRequest;
    public final Set<Header> corsHeaders = new LinkedHashSet<>();

    //-----------------------------------------------------------------------
    public final ContentDisposition contentDisposition;
    public ServletOutputStreamEx outputStream; // strumien wyjsciowy
    public final String[] path;
    private Boolean logsEnabled = null;

    Boolean compression;
    public EndpointMapping endpointMapping;

    public HttpRequest(final HttpServletRequest request, final HttpServletResponse response) {
        this.request = request;
        this.response = response;
        headers.caseSensitive = false;

        /*
        http://localhost:9898/ctx/$/sessions

        contextPath = /ctx
        relativePath = /$/sessions
        RequestURI = /ctx/$/sessions
         */
        final boolean incorrectConfigPath;
        {
            String sPath = Utils.coalesce(request.getPathInfo(), request.getRequestURI(), "");
            String ctx = WebAppServer.context != null ? WebAppServer.context.getContextPath() : "/";

            incorrectConfigPath = Objects.equals(sPath, ctx);

            if (sPath.isEmpty() || !sPath.startsWith(ctx))
                throw new RuntimeException(LServlet.CONTEXT_PATHS_MISMATCH.toString());

            ctx = new Str(ctx).removePrefix("/").removeSufix("/").toString();
            sPath = new Str(sPath).removePrefix("/").toString();;

            this.contextPath = ctx;

            // sytuacja w której mamy zdefniowaną ścieżkę kontekstu i bieżące żądanie wskazuje na plik a nie na kontekst
            // np kontekst = ctx i url = http://localhost/ctx  (bez '/' na końcu)
            sPath = sPath.substring(ctx.length());
            relativePath = sPath.startsWith("/") ? sPath.substring(1) : sPath;
        }

        method = HttpMethod.get(request.getMethod());
        isAjaxRequest = "XMLHttpRequest".equals(getHeader("X-Requested-With"));

        isCorsRequest = isAjaxRequest
                && !getHeader("Origin", "").isEmpty()
                && !getHeader("Host", "").isEmpty()
                && !getHeader("Origin", "").equals(getHeader("Host", ""));

        corsHeaders.add(Header.CONTENT_TYPE);
        corsHeaders.add(Header.X_REQUESTED_WITH);
        corsHeaders.add(Header.X_REQUESTED_EVAL);
        corsHeaders.add(Header.X_REQUESTED_PARAMS);
        corsHeaders.add(Header.X_REQUESTED_SKIP_LOG);
        corsHeaders.add(Header.PARENT_REQUEST_ID);
        corsHeaders.add(Header.ERROR);

        userAgent = new UserAgent(getHeader("User-Agent"));
        url = new Url(this.request.getRequestURL()
                + (this.request.getQueryString() != null
                ? "?" + this.request.getQueryString() : ""))
                .readOnly();
        url.contextPath = request.getContextPath();

        try {
            this.request.setCharacterEncoding("UTF-8");
            this.response.setCharacterEncoding("UTF-8");
        } catch (UnsupportedEncodingException ex) {
            throw new ThrowableException(ex);
        }

        path = relativePath.split("/");

        {
            Enumeration<String> hdrs = request.getHeaderNames();
            while (hdrs.hasMoreElements()) {
                String name = hdrs.nextElement();
                Enumeration<String> hh = request.getHeaders(name);
                while (hh != null && hh.hasMoreElements())
                    headers.add(name, hh.nextElement());
            }
        }

        /* to samo co this.request.getParameterMap()
         if (this.request.getQueryString() != null)
         for (String item : this.request.getQueryString().split("&")) {
         String[] pair = item.split("=");
         try {
         String name = pair.length > 0
         ? URLDecoder.decode(pair[0], "UTF-8") : null;
         String value = pair.length > 1
         ? URLDecoder.decode(pair[1], "UTF-8") : null;
         params.add(RequestParams.RequestParamSource.url, name, value);
         } catch (Exception e) {
         Log.error(e);
         }
         }
         */
        for (Params.Param p : new Params().parseQuery(getHeaderUri("X-Requested-Params", "")))
            params.add(p.name, p.value);

        queryParams.parseQuery(this.request.getQueryString());
        // getParameterMap odczytuje calego input streama, pobiera czesci w przypadku multiparta
        for (Map.Entry<String, String[]> ee : this.request.getParameterMap().entrySet())
            for (String s : ee.getValue())
                params.add(ee.getKey(), s);

        session = BaseSession.getInstance(this);

        Language lang = CHttp.useLanguageCookie.value() ? Languages.get(getCookie("language")) : null;
        if (lang != null && session.language.get() != lang) {
            Log.debug("Ustawiam język \"" + lang.name + "\" na podstawie ciastka");
            session.language.set(lang);
        }

        if (lang == null && CHttp.autoDetectlanguage.value())
            Is.nonEmptyR(getHeader("Accept-Language"), (hdr) -> {

                LinkedHashMap<String, Double> map = new LinkedHashMap<>();

                for (String ss : hdr.split(",")) {
                    String[] split = ss.split(";");
                    String l = split[0];
                    Double q = 1.0;
                    if (split.length > 1) {
                        String s = split[1].trim().replace(" ", "");
                        if (s.startsWith("q="))
                            q = Utils.strDouble(s.substring(2), 0d);
                    }
                    map.put(l, q);
                }

                Utils.sortMap(map, (o1, o2) -> Double.compare(o2.getValue(), o1.getValue()));

                Language sl = session.language.get();
                for (String sLang : map.keySet()) {
                    Language ll = Languages.get(sLang);
                    if (ll == null)
                        continue;

                    if (sl == ll)
                        break;

                    Log.debug("Ustawiam język \"" + ll.name + "\" na podstawie nagłówka Accept-Language");
                    session.language.set(ll);
                }

                return hdr;
            });

        proxyName = new Strings()
                .add(getHeader("via"))
                .add(getHeader("x-forwarded-for"))
                .toString(", ");

        parentRequestId = getHeader("Parent-Request-Id");
        contentDisposition = new ContentDisposition(this);

        String cType = null;
        if (request.getContentType() != null)
            cType = request.getContentType().toLowerCase().trim();
        contentType = cType;

        String ct = Utils.coalesce(request.getContentType(), "").toLowerCase();

        multiPart = ct.contains("multipart/form-data") || ct.contains("multipart/mixed stream");

        if (incorrectConfigPath)
            try {
                Log.debug("Redirect", "/" + contextPath + "/");
                response.sendRedirect("/" + contextPath + "/");
            } catch (IOException ex) {
                Log.error(ex);
            }
    }

    public Collection<Part> getParts() throws IOException, ServletException {
        return request.getParts();
    }

    public String getHeaderUri(String name, String def) {
        String hdr = request.getHeader(name);
        if (hdr == null)
            return def;
        return StrUtils.decodeURIComponent(hdr);
    }

    public ServletInputStreamEx getInputStream() throws IOException {
        if (inputStream == null)
            inputStream = new ServletInputStreamEx(this);
        return inputStream;
    }

    public long contentLength() {
        return request.getContentLength();
    }

    public static HttpRequest getInstance() {
        return getInstance(Thread.currentThread());
    }

    public static HttpRequest getInstance(Thread thread) {
        synchronized (HRequests.requests) {
            return HRequests.requests.get(thread);
        }
    }

    @Override
    public String toString() {
        return request.getMethod() + ": " + url;
    }

    public HttpRequest setHeader(String name, String value) {
        response.setHeader(name, value);
        return this;
    }

    public void returnFile(CachedData cd) throws IOException {
        String name = cd.retrunName;
        if (name == null || name.isEmpty())
            name = cd.getFileName();
        try (InputStream fis = cd.getInputStream()) {
            returnFile(fis, name, fis.available(), cd.mimeType, cd.eTag);
        }
    }

    public void returnFile(File file)
            throws IOException {
        try (BufferedInputStream fis = new BufferedInputStream(new FileInputStream(file))) {
            returnFile(fis, file.getName(), fis.available(), null, null);
        }
    }

    public void returnFile(File file, String eTag)
            throws IOException {
        try (BufferedInputStream fis = new BufferedInputStream(new FileInputStream(file))) {
            returnFile(fis, file.getName(), fis.available(), null, eTag);
        }
    }

    public void returnFile(byte[] buffer, String fileName, String mime,
            String eTag)
            throws IOException {
        returnFile(new ByteArrayInputStream(buffer), fileName, buffer.length, mime, eTag);
    }

    public void returnFile(byte[] buffer, String fileName)
            throws IOException {
        returnFile(new ByteArrayInputStream(buffer), fileName, buffer.length, null, null);
    }

    public void returnFile(InputStream in, String fileName)
            throws IOException {
        int aval = in.available();
        returnFile(in, fileName, aval >= 0 ? aval : null, null, null);
    }

    public OutputStream returnFile(String fileName, Integer size, String mime,
            String eTag) throws IOException {
        processResponse(fileName, mime, eTag);
        if (size != null)
            response.setContentLength(size);
        return getOutputStream();
    }

    public void returnFile(InputStream in, String fileName, Integer size, String mime,
            String eTag) throws IOException {

        logRequest();

        processResponse(fileName, mime, eTag);

        int aval = in.available();
        if (size == null)
            size = aval;

        if (aval > 0 && aval != size)
            Log.warning("Różnica rozmiarów zwracabych strumieni\nDeklarowany: "
                    + size + " (" + Utils.formatSize(size) + ")\nRzeczywisty: "
                    + aval + " (" + Utils.formatSize(aval) + ")");

        long min = 0;
        long max = size - 1;

        // obsługa zakresów:
        setHeader("Accept-Ranges", "bytes");
        String sRange = getHeader("range");
        if (sRange != null && sRange.contains("bytes")) {

            sRange = sRange.replace(" ", "");
            sRange = sRange.substring(sRange.indexOf("=") + 1);

            if (!sRange.contains("-"))
                max = Utils.strInt(sRange, (int) max);
            else {
                min = Utils.strInt(sRange.substring(0, sRange.indexOf("-")), 0);
                max = Utils.strInt(sRange.substring(sRange.indexOf("-") + 1), (int) max);
            }

            setHeader("Content-Range", "bytes " + min + "-" + max + "/" + size);
            setStatus(206); //częściowa treść

            new Log(LogKind.REQUEST)
                    .tag("Response", "Part")
                    .value("206, Part: " + Utils.formatSize(min) + " -  "
                            + Utils.formatSize(max) + (!fileName.isEmpty()
                            ? ", " + fileName : ""))
                    .comment(Utils.formatSize(max - min))
                    .send();
        }

        OutputStream op = getOutputStream();
        response.setContentLength((int) (1 + max - min));

        int total = 0;
        try {

            byte[] bbuf = new byte[102400];
            try {
                if (min > 0)
                    total = (int) in.skip(min);
//                while (total < min) {
//                    int read = in.read();
//                    if (read == -1)
//                        break;
//                    ++total;
//                }

//                TConsole.print("\n\nth " + Thread.currentThread().getId());
//                TConsole.print("min " + Utils.formatSize(min));
//                TConsole.print("max " + Utils.formatSize(max));
//                TConsole.print("diff " + Utils.formatSize(max - min));
                while (true) {
                    if (total + bbuf.length > max)
                        bbuf = new byte[(int) (1 + max) - total];

                    int length = in.read(bbuf);

                    if (length <= 0)
                        break;

                    op.write(bbuf, 0, length);
                    total += length;

                }
                op.flush();
            } finally {
                in.close();
                try {
                    op.close(); // spróbuj zamknąć strumień
                } catch (IOException e) {
                }
            }

            // wytlumnienie bledu powstalego w przypadku anulowania pobierania
        } catch (IOException ex) {
            if (!(ex instanceof EOFException) && !"ClientAbortException".equals(ex.getClass().getSimpleName()))
                throw new IOError(ex);
            if (size > 0 && total != size)
                Log.warning("Response", "Abort, " + 100 * total / size + "% " + fileName);
        }

    }

    public ServletOutputStreamEx getOutputStreamFile(String fileName,
            String contentType, Integer length) throws IOException {
        setContentType(contentType);
        contentDisposition.setHeader(fileName);
        if (length != null)
            response.setContentLength(length);
        return (ServletOutputStreamEx) getOutputStream();
    }

    private void processResponse(String fileName, String mime,
            String eTag) throws IOException {

        if (fileName == null)
            fileName = "";
        fileName = fileName.replace("\\", "/").trim();

        if (!fileName.isEmpty()) {

            if (fileName.contains("?"))
                fileName = fileName.substring(0, fileName.indexOf("?"));

            if (fileName.endsWith("/"))
                fileName = fileName.substring(0, fileName.length() - 1);

            if (fileName.contains("/"))
                fileName = fileName.substring(fileName.lastIndexOf("/") + 1);

            if (mime == null || mime.equals("")) {
                mime = WebAppServer.context.getMimeType(fileName);
                if (mime == null)
                    mime = "application/octet-stream";
            }
        }
        contentDisposition.setHeader(fileName);
        setContentType(mime);
        if (eTag != null && !eTag.isEmpty()) {
            setHeader("ETag", eTag);
            setHeader("Pragma", "public");
            setHeader("Cache-Control", "public");
        } else {
            setHeader("Pragma", "no-cache");
            setHeader("Cache-Control", "no-cache, no-store, must-revalidate");
            response.setDateHeader("Expires", 0);
        }

    }

    public String getHeader(String name) {
        return request.getHeader(name);
    }

    public String getHeader(String name, String defaultValue) {
        return Utils.coalesce(request.getHeader(name), defaultValue);
    }

    public boolean isLocked() {
        return response.isCommitted();
    }

    public OutputStream getOutputStream() throws IOException {
        // if (page == null)
        //   return super.getOutputStream();
        if (outputStream != null)
            return outputStream;

        ControllerEndpoint<?> endpoint = controller != null ? controller.endpoint() : null;
        if (endpoint != null)
            for (BeforeReturnContent event : endpoint.beforeReturnContent)
                event.call(endpoint, this);

        outputStream = new ServletOutputStreamEx(this);
        return outputStream;
    }

    public PrintWriter getWriter(Charset charset) throws IOException {
        try {
            if (charset == null)
                charset = Charset.forName(response.getCharacterEncoding());
            return new PrintWriter(
                    new OutputStreamWriter(getOutputStream(), charset));

        } catch (Exception e) {
            Log.error(e);
            throw e;
        }
    }

    public HttpRequest setStatus(int status) {
        response.setStatus(status);
        return this;
    }

    public HttpRequest setContentType(String contentType) {
        response.setContentType(contentType);
        return this;
    }

    public HttpRequest setHeaderUri(String name, String value) {
        setHeader(StrUtils.encodeURIComponent(name), StrUtils.encodeURIComponent(value));
        return this;
    }

    public void redirect(Class<? extends Controller> page) throws IOException {
        redirect(new Url(page));
    }

    public void redirect(Url location) throws IOException {
        if (location == null)
            return;
        if (location.toString().startsWith("?"))
            location = new Url("/" + this.relativePath + location.toString());

        Log.debug("Redirect", location);
        response.sendRedirect(getRelativePath(location).toString());
    }

    public Url getAbsolutePath(String path) {
        return getAbsolutePath(path, null);
    }

    public Url getAbsolutePath(String path, Boolean https) {
        return url.getAbsolutePath(path, https);
    }

    public int getCookie(String name, int def) {
        return Utils.strInt(getCookie(name), def);
    }

    public String getCookie(String name, String def) {
        String s = getCookie(name);
        return s == null ? def : s;
    }

    public String getCookie(String name) {
        String result = null;
        Cookie[] cookies = request.getCookies();

        int cnt = 0;

        if (cookies != null)
            for (int i = 0; i < cookies.length; i++)
                if (cookies[i].getName().equals(name))
                    try {
                        String val = cookies[i].getValue();
                        result = val != null ? URLDecoder.decode(
                                val, parametersEncoding) : null;
                        ++cnt;
                    } catch (Exception ex) {
                        Log.warning(ex);
                        return null;
                    }
        if (cnt > 1)
            Dev.warning("Cookie", "Znaleziono " + cnt + " ciastka o nazwie " + name);
        return result;
    }

    public boolean removeCookie(String name) {
        Cookie[] cookies = request.getCookies();
        if (cookies == null)
            return false;
        for (Cookie cookie : cookies)
            if (cookie.getName().equals(name)) {
                cookie.setMaxAge(0);
                cookie.setValue(null);
                response.addCookie(cookie);
                return true;
            }
        return false;
    }

    public void addExceptionHeader(Throwable e) {
        ErrorMessage msg = Handlers.errors.getInstance().getMessage(e);

        JArray jerr = new JArray();
        jerr.options.compactMode(true);

        jerr.add(EError.addException(e))
                .add(msg.title)
                .add(msg.message);

        JArray jdet = new JArray();
        for (Entry<String, String> en : msg.details.entrySet())
            jdet.array().add(en.getKey()).add(en.getValue());

        if (!jdet.isEmpty())
            jerr.add(jdet);

        setHeaderUri("Error", jerr.toString());
    }

    public Url getRelativePath(Class<? extends Controller> page) {
        return getRelativePath(new Url(page));
    }

    public Url getRelativePath(Url url) {
        if (url == null)
            return null;
        return getRelativePath(url.toString());
    }

    public Url getRelativePath(String path) {
        // aby ścieżka została zamieniona na relatywną, musi zaczynać się od /
        if (path == null)
            return null;

        if (path.isEmpty()
                || path.contains("://")
                || !path.startsWith("/")
                || path.startsWith("?")
                || path.startsWith("#"))
            return new Url(path);

        String s = new Str(path).removePrefix("/").toString();

        //   this.request.getContextPath();
        String spath = relativePath;

        if (spath.startsWith("/"))
            spath = spath.substring(1);

        if (!spath.contains("/"))
            return new Url(s);

        String ctx = new Str(Utils.coalesce(request.getContextPath(), ""))
                .removeSufix("/")
                .toString();

        String source = request.getHeader("parent");
        if (source == null)
            source = url.toString();

        if (source != null && source.contains(ctx)) {
            String uri = ctx.isEmpty() ? spath
                    : source.substring(source.indexOf(ctx) + ctx.length() + 1);

            while (uri.contains("/")) {
                uri = uri.substring(uri.indexOf("/") + 1);
                s = "../" + s;
            }
        }

        return new Url(s);
    }

    public void returnDirectoryAsZip(File dir, boolean includeSubdirs,
            String outName) throws IOException {
        if (dir == null)
            return;
        if (outName == null)
            outName = dir.getName() + ".zip";

        SearchFiles search = new SearchFiles(dir.toString(), includeSubdirs);
        returnFilesAsZip(search.getFiles(), dir.getParentFile(), outName);
    }

    public void returnFilesAsZip(List<File> files, File rootPath, String outName)
            throws IOException {
        if (files == null)
            return;
        if (outName == null && rootPath != null)
            outName = rootPath.getName() + ".zip";
        if (outName == null)
            outName = "files.zip";

        setContentType("application/zip");
        setHeader("Content-Disposition", "attachment; filename="
                + StrUtils.convertPolishChars(outName));

        OutputStream out = getOutputStream();
        ZipOutputStream zos = new ZipOutputStream(out);
        try {
            zos.setLevel(9);

            for (File file : files) {
                Path path = new Path(file);
                if (rootPath != null)
                    path = path.getRelativePath(new Path(rootPath));

                ZipEntry ze = new ZipEntry(path.toString());
                zos.putNextEntry(ze);
                IOUtils.copy(file, zos);
                zos.closeEntry();
            }
        } finally {
            zos.finish();
            zos.close();
        }
    }

    public void setAttribute(String name, Object object) {
        request.setAttribute(name, object);
    }

    public Object getAttribute(String name) {
        return request.getAttribute(name);
    }

    public boolean isSecure() {
        return request.isSecure();
    }

    public String getQueryString() {
        return request.getQueryString();
    }

    public HttpMethod getMethod() {
        return HttpMethod.get(request.getMethod());
    }

    public void addHeader(String key, String val) {
        response.addHeader(key, val);
    }

    public void setCompression(Boolean compression) {
        this.compression = compression;
    }

    public String contentType() {
        return request.getContentType();
    }

    /**
     * Ustawia domyślny naglowek
     */
    public void setNoChacheHeader() {
        setHeader("Pragma", "no-cache");
        setHeader("Cache-Control", "no-cache, no-store, must-revalidate");
        setHeader("Expires", "0");
    }

    public void returnPlainText(String str) {
        returnPlainText(str, 200);
    }

    public void returnPlainText(String str, int resultCode) {
        setStatus(resultCode);
        returnCustom(str, "text/plain; charset=UTF-8");
    }

    public void return304NoChange() {
        try {
            if (isLocked()) {
                Log.warning("Response is locked");
                return;
            }
            setStatus(304);
            try (OutputStream out = getOutputStream()) {
                out.flush();
            }
        } catch (Exception ex) {
            Log.error(ex);
        }
    }

    public void returnCustom(String str, String contentType) {
        try {
            if (isLocked()) {
                Log.warning("Response is locked");
                return;
            }
            setNoChacheHeader();
            if (contentType == null)
                contentType = "application/octet-stream";
            setContentType(contentType);

            try (PrintWriter writer = getWriter(Utils.UTF8)) {
                if (str != null)
                    writer.write(str);
                writer.flush();
            }
        } catch (Exception ex) {
            Log.error(ex);
        }
    }

    /**
     * Zwraca akcję JS, która po odebraniu zostanie automatycznie wykonana
     *
     * @param actions
     */
    public void returnAction(JsAction... actions) throws Http405MethodNotAllowed {
        if (!isAjaxRequest)
            throw new Http405MethodNotAllowed(LServlet.REQUENST_NOT_INITIALIZED_BY_AJAX.toString());

        JsActions acts = new JsActions(actions);
        Element tag = acts.getTag();
        if (tag != null)
            tag.getHTML().head.link(Res.utils);
        setHeader("X-Requested-Eval", "true");
        returnCustom(acts.toString(), "application/javascript; charset=UTF-8");
    }

    public void returnHTML(String str) {
        returnCustom(str, "text/html; charset=UTF-8");

    }

    public void returnJson(JCollection json) {
        setNoChacheHeader();
        returnCustom(json.toString(), "application/json; charset=UTF-8");
    }

    public void returnXML(XML xml, int status) {
        try {
            if (isLocked()) {
                Log.warning("Response is locked");
                return;
            }

            setNoChacheHeader();
            setStatus(status);
            setContentType("text/xml; charset=UTF-8");

            try (PrintWriter writer = getWriter(Utils.UTF8)) {
                xml.write(writer);
                writer.flush();
            }
        } catch (Exception ex) {
            Log.error(ex);
        }
    }

    public void returnHTML(Element tag, int status) {
        try {
            ControllerEndpoint<?> endp = controller != null ? controller.endpoint() : null;
            if (endp != null)
                for (BeforeReturnHtml event : endp.beforeReturnHtml)
                    event.call(endp, tag, status);

            boolean xHtml = (tag instanceof Html) && CContent.xhtmlMode.value(false);
            if (tag instanceof Html && ((Html) tag).xHTML != null)
                xHtml = ((Html) tag).xHTML;

            boolean compact = CContent.compactMode.value(false);
            if (tag instanceof Html && ((Html) tag).compact != null)
                compact = ((Html) tag).compact;

            if (AppContext.devMode && tag instanceof Html)
                ((Html) tag).validate(xHtml);

            if (tag instanceof Html)
                ((Html) tag).head.optimize(xHtml);

            if (isLocked()) {
                Log.warning("Response is locked");
                return;
            }

            setNoChacheHeader();
            setStatus(status);

            setContentType(xHtml
                    ? "application/xhtml+xml; charset=UTF-8"
                    : "text/html; charset=UTF-8");

            // OutputStream out = getOutputStream();
            try (StrWriter writer = new StrWriter(
                    //  new BufferedWriter(new OutputStreamWriter(new FileOutputStream("R:/page.html"))),
                    new BufferedWriter(getWriter(Utils.UTF8), 1024 * 100))) {

                writer.memoryCopy(AppContext.devMode);

                if (tag instanceof Html)
                    ((Html) tag).getContent(writer, xHtml, compact);
                else
                    tag.getContent(writer);

                writer.flush();
            }
        } catch (Exception ex) {
            Log.error(ex);
            throw new ThrowableException(ex);
        }
    }

    public void setLogsEnabled(boolean logsEnabled) {
        this.logsEnabled = logsEnabled;
    }

    public boolean logsEnabled() {
        return logsEnabled != null ? logsEnabled : true;
    }

    private boolean requestLogged = false;
    private boolean responseLogged = false;

    public void logRequest() {
        if (requestLogged || Boolean.FALSE.equals(logsEnabled))
            return;
        ServletLog.requestInfo(this).send();
        requestLogged = true;
    }

    public void logResponse() {
        if (responseLogged || Boolean.FALSE.equals(logsEnabled))
            return;
        ServletLog.responseInfo(this).send();
        responseLogged = true;
    }

    public void setCookie(String name, String value, Interval expireTime) {
        Cookie cookie = new Cookie(name, value);
        if (expireTime != null)
            cookie.setMaxAge((int) expireTime.getTime(Unit.SECONDS));
        cookie.setPath("/" + contextPath);
        response.addCookie(cookie);
    }

    public void addCorsHeaders() {
        setHeader("Access-Control-Allow-Origin", getHeader("Origin"));
        setHeader("Access-Control-Allow-Credentials", "true");

        Strings simple = new Strings();
        Strings expose = new Strings();

        for (Header h : corsHeaders)
            if (h != null)
                if (h.expose)
                    expose.add(h.name);
                else
                    simple.add(h.name);

        if (!simple.isEmpty())
            setHeader("Access-Control-Allow-Headers", simple.toString(", "));
        if (!expose.isEmpty())
            setHeader("Access-Control-Expose-Headers", expose.toString(", "));
    }

    public Url toAbsoluteUrl(Boolean slash) {
        String url = request.getRequestURL().toString();

        if (!Is.empty(request.getQueryString()))
            url += "?" + request.getQueryString();

        String query = "";
        if (url.contains("?")) {
            query = url.substring(url.indexOf("?"));
            url = url.substring(0, url.indexOf("?"));
        }

        if (Boolean.FALSE.equals(slash) && url.endsWith("/"))
            url = url.substring(0, url.length() - 1);

        if (Boolean.TRUE.equals(slash) && !url.endsWith("/"))
            url += "/";

        return new Url(url + query);
    }

}

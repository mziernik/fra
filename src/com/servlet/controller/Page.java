package com.servlet.controller;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.user.BaseUserData;
import com.utils.text.StrWriter;
import com.exceptions.http.Http410GoneException;
import com.exceptions.http.Http404FileNotFoundException;
import com.exceptions.http.Http400BadRequestException;
import com.json.Escape;
import com.xml.XML;
import com.utils.collections.Params;
import com.context.AppContext;
import com.cache.CachedData;
import com.mlogger.Log;
import com.context.index.Index;
import com.exceptions.ThrowableException;
import com.html.core.Html;
import com.html.core.tag.*;
import com.html.js.*;
import com.json.*;
import com.lang.LServlet;
import com.resources.Res;
import com.resources.core.html.ScriptFile;
import com.servlet.controller.intf.ControllerEvent;
import com.servlet.interfaces.*;
import com.servlet.requests.*;
import com.servlet.views.ViewController;
import com.servlet.views.ViewControllerMeta;
import com.utils.*;
import com.utils.collections.Params.Param;
import com.utils.reflections.TMethod;
import java.io.*;
import java.util.*;
import javax.servlet.http.Part;

public abstract class Page implements Controller {

    public final BaseSession session;
    public final BaseUserData user;
    public static int uploadFileExpires = 1800;
    public final HttpRequest request;
    public final Html html;
    public final Head head;
    public final Body body;
    public boolean useAbsolutePaths; // flaga wymusza użycie flag absolutnych zamiast relatywnych
    public final LinkedList<CachedData> uploadedFiles = new LinkedList<>();
    public final RequestParams params;
    // ToDo: przerobic konstruktor aby nie przyjmowalo parametru view
    private Integer keepAlive;

    protected void onKeepAlive() throws Exception {

    }

    @SuppressWarnings("unchecked")
    public Page() {
        try {
            ControllerEndpoint meta = endpoint();
            meta.createHtml.add((ControllerEvent) this::createHtml);

            this.request = meta.http;
            html = new Html(this);
            head = html.head;
            body = html.body;
            html.node.addBuildListener((Html tag) -> {
                addRequiredResources();
                return true;
            });
            session = meta.http.session;
            user = session.user;
            params = request.params;
        } catch (Exception e) {
            throw new ThrowableException(e);
        }
    }

    private void createHtml(ControllerEndpoint ctrl) {
        if (!endpoint().title().isEmpty())
            head.title(endpoint().title());

        List<Function> functs = new LinkedList<>();
        List<JsMethod.JsMethodImpl> jsMethods = new LinkedList<>();

        jsMethods.addAll(endpoint().index.jsMethods);

        // dodaj listę metod z widoku
        ViewControllerMeta view = ViewControllerMeta.get(endpoint().view());
        if (view != null)
            jsMethods.addAll(view.jsMethods);

        for (JsMethod.JsMethodImpl jsMethod : jsMethods) {

            Function funct = new Function(jsMethod.name);
            for (Arg.ArgMeta arg : jsMethod.method.arguments)
                funct.param(arg.name);
            funct.body((view != null ? "view." : "$")
                    + "call('" + jsMethod.name + "'"
                    + (jsMethod.method.arguments.length > 0 ? ", " : "")
                    + jsMethod.method.getArgumentNames().toString(", ") + ");"
            );
            functs.add(funct);
        }

        if (!functs.isEmpty())
            body.script(functs.toArray(new Function[0]));
    }

    private void onDone(ControllerEndpoint ctrl, TMethod method) throws Exception {
        if (!http().isLocked())
            returnHTML();
    }

    private void addRequiredResources() {

        try {

            Class<? extends ViewController> view = endpoint().view();

            if (view != ViewController.class) {

                head.link(Res.utils);
                head.script(new OnLoadDoc(new Eval(
                        "window.view = new JsonSocket('" + ViewControllerMeta.getId(view) + "', true);"
                )));

            }

            String flags = "L";
            // if (standby)
            //      flags += "S";
            if (AppContext.devMode)
                flags += "D";

            String http = request.getAbsolutePath("/", null).toString();
            String https = request.getAbsolutePath("/", true).toString();

            // json.options.singleQuote(true);
//            html.head.meta("$service", new StrWriter()
//                    .append(request.id).append("|")
//                    .append("").append("|") //ToDo Usunąć
//                    .append(flags).append("|")
//                    .append(http).append("|")
//                    .append(https.equals(http) ? "" : https).append("|")
//                    .append(keepAlive != null ? keepAlive : "").toString());

            /*

             // sb.append(" });");
             TempResource res = new TempResource(30, "context.js", content.getBytes("UTF-8"));
             res.id = "ctx_" + res.id + ".js";
             if (BaseContext.debugMode)
             res.id = "context.js";
             page.head.linkJavaScript("/" + res.id);
             */
            //  head.script("addEventListener('load', function(){ service.keepAlive(" + time + ") });");
        } catch (Exception e) {
            Log.error(e);
        }
    }

    private void onUpload(boolean ajax) throws Exception {

        if (!(this instanceof UploadTarget)) {
            Log.warning("Klasa " + getClass().getName() + " nie implementuje "
                    + UploadTarget.class.getName()
                    + ".\nUploadowana treść zostanie zignorowana");
            return;
        }

        if (ajax) {
            String filename = request.getHeaderUri("ajax-upload", "");

            CachedData cd = new CachedData("Upload", "ajax-upload", filename,
                    uploadFileExpires, request.getInputStream());

            for (Param p : new Params().parseQuery(request.getHeaderUri("ajax-cache-params", null)))
                cd.attribute(p.name, p.value);

            http().setHeader("Cached-File-Id", cd.key);
            http().setHeader("Cached-File-Size", "" + cd.length());
            uploadedFiles.add(cd);
            request.setHeader("Upload-Cache-Id", cd.key);
            ((UploadTarget) this).onUploadFile(cd, "ajax-upload", null);

        }

        if (!ajax) {
            Collection<Part> parts = request.getParts();
            if (parts != null)
                for (Part part : parts)
                    try (InputStream is = part.getInputStream()) {
                        CachedData cd = new CachedData("upload", "web-upload", part.getName(),
                                uploadFileExpires, is);
                        request.addHeader("Upload-Cache-Id", cd.key);
                        ((UploadTarget) this).onUploadFile(cd, part.getName(), part);
                    }

        }

    }

    public void returnPlainText(String str) {
        request.returnPlainText(str);
    }

    public void returnPlainText(String str, int resultCode) {
        request.returnPlainText(str, resultCode);
    }

    public void return304NoChange() {
        request.return304NoChange();
    }

    public void returnCustom(String str, String contentType) {
        request.returnCustom(str, contentType);
    }

    public void returnHTML(String str) {
        request.returnHTML(str);
    }

    public void returnHTML(Element tag) {
        request.returnHTML(tag, 200);
    }

    public void returnHTML() {
        request.returnHTML(html, 200);
    }

    public void returnHTML(int status) {
        request.returnHTML(html, status);
    }

    public void returnJson(JCollection json) {
        request.returnJson(json);
    }

    public void returnXML(XML xml, int status) {
        request.returnXML(xml, status);
    }

    public void link(ScriptFile... files) {
        head.link(files);
    }

    public void link(String... files) {
        head.link(files);
    }

    public boolean returnCacheFileById(String key, boolean removeFile)
            throws IOException, Http404FileNotFoundException, Http410GoneException {
        CachedData cd = CachedData.get(key);
        if (cd != null) {
            returnCacheData(cd);
            if (removeFile)
                cd.delete();
            return true;
        }
        throw new Http404FileNotFoundException(null, "id: " + key);
    }

    public void returnCacheDataFileId(CachedData cd) {
        returnCacheDataFileId(cd, "");
    }

    public void returnCacheDataFileId(CachedData cd, String plainText) {
        http().setHeader("Cached-File-Id", cd.key);
        http().setHeader("Cached-File-Size", "" + cd.length());
        http().setHeader("Cached-File-Name", cd.name);
        http().setHeader("Cached-File-MimeType", cd.getMimeType());
        returnPlainText(plainText);
    }

    public void returnCacheData(CachedData cd) throws IOException {
        returnCacheData(cd, 0, false);
    }

    public void returnCacheData(CachedData cd, Integer expireSeconds,
            boolean previewMode) throws IOException {

        if (request.contentDisposition.inline == null)
            request.contentDisposition.inline = false;

        String sname = previewMode ? null : cd.name;
        // sprobuj przepisac sformatowana nazwe pliku jesli istnieje
        if (!previewMode && cd.retrunName != null && !cd.retrunName.isEmpty()) {
            sname = cd.retrunName;

            // jesli returnName nie zawiera rozszerzenia to przepisz z oryginalu
            if (!sname.contains(".") && cd.name.contains("."))
                sname += cd.name.substring(cd.name.lastIndexOf("."));
        }

        String mm = cd.getMimeType();
        if (mm != null)
            if (mm.equalsIgnoreCase("application/xhtml+xml")
                    || mm.equalsIgnoreCase("application/html")) {
                returnHTML(new String(cd.getData(), Utils.UTF8));
                return;
            }

        try (InputStream in = cd.getInputStream()) {
            request.returnFile(in, sname, (int) cd.length(), cd.mimeType, cd.eTag);
        }
    }

    public ServletOutputStreamEx getOutputStreamFile(String fileName,
            String contentType, Integer length) throws IOException {
        request.setContentType(contentType);
        request.setHeader("Content-Disposition", "attachment; filename="
                + StrUtils.convertPolishChars(fileName));
        if (length != null)
            request.response.setContentLength(length);
        return (ServletOutputStreamEx) request.getOutputStream();
    }

//    // </editor-fold>
//    public int strInt(String value, int def) {
//        int result;
//        try {
//            result = Integer.parseInt(value);
//        } catch (NumberFormatException e) {
//            result = def;
//        }
//        return result;
//    }
//
//    public double strDouble(String value, double def) {
//        double result;
//        try {
//            result = Double.parseDouble(value);
//        } catch (NumberFormatException e) {
//            result = def;
//        }
//        return result;
//    }
//    public String getHeader(String name) {
//        String header = request.getHeader(name);
//        if (header == null)
//            return null;
//        return StrUtils.decodeURIComponent(header);
//    }
//    public void setCustomResponseHeader(String name, String value) {
//        request.setHeader(escapeURI(name), escapeURI(value));
//    }
    /*
     public void redirectError(Throwable e, int errorNumber, String jspFile) {
     // request.setStatus(errorNumber);
     // request.sendError(errorNumber);

     if (response.isOutputStreamLocked()) {
     return;
     }

     request.setAttribute("error_code", errorNumber);
     request.setAttribute("javax.servlet.jsp.jspException", e);

     if (BaseContext.config.debugMode()) {
     String par = "";

     for (Param p : params_old) {
     par += "<li>" + HtmlBuilder.escape(p.name, true, false)
     + ": " + HtmlBuilder.escape(p.value, true, false) + "</li>";
     }

     addExceptionHeader(e, request, response);

     request.setAttribute("request_params", par);

     if (e instanceof HttpException) {
     HttpException he = (HttpException) e;
     request.setAttribute("Error-Details", he.details);
     }

     }

     MainServlet.logResponse(this);
     request.lockOutputStream();
     try {
     request.getSession().getServletContext().getRequestDispatcher(jspFile).forward(request,
     response);
     } catch (ServletException | IOException ex) {
     Log.warning(ex);
     }
     }
     */
    /**
     * Generuje UID, 32 znaki bez myslnikow
     */
    public static String getUID() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    /**
     * Pobierz aktualny adres url bieżącej strony
     */
    public Url getAbsoluteURL() {
        String url = this.getClass().getSimpleName();
        if (endpoint().url().length > 0)
            url = endpoint().url()[0];
        return request.getAbsolutePath(url, null);
    }

    public Url getAbsoluteURL(String url, Boolean https) {
        return request.getAbsolutePath(url, https);
    }

    public Url getAbsoluteURL(Url url, Boolean https) {
        return request.getAbsolutePath(Utils.toString(url), https);
    }

    public Url getAbsoluteURL(String url) {
        return request.getAbsolutePath(url, null);
    }

    public Url getRelativeURL(String url) {
        return request.getRelativePath(url);
    }

    public Url getRelativeURL(Url url) {
        return request.getRelativePath(url);
    }

    public Url getRelativeURL(Class<? extends Controller> page) {
        return request.getRelativePath(new Url(page));
    }

    public static Url getUrl(String href) {

        if (href == null || href.toLowerCase().startsWith("http://")
                || href.toLowerCase().startsWith("https://"))
            return new Url(href);

        Controller ctrl = Controller.getInstance(false);
        Page page = ctrl instanceof Page ? (Page) ctrl : null;

        if (page == null) {
            HttpRequest req = HttpRequest.getInstance();
            if (req != null)
                return req.getRelativePath(href);
            return new Url(href);
        }

        return page.useAbsolutePaths ? page.request.getAbsolutePath(href)
                : page.request.getRelativePath(href);
    }

    /**
     * Pobiera alias adresu URL danej klasy
     */
    public String getAlias() {
        return getAlias(this.getClass());
    }

    public static String getAlias(Class<? extends Page> cls) {
        for (ControllerMetaData pc : Index.controllers)
            if (pc.controller == cls && !pc.urls.isEmpty())
                return pc.urls.get(0);
        return null;
    }

    protected CachedData getUploadedFile() throws Http400BadRequestException {
        for (CachedData cd : uploadedFiles)
            if (!cd.name.isEmpty())
                return cd;
        throw new Http400BadRequestException(LServlet.FILE_NOT_ADDED.toString(), null);
    }

    /**
     * Ustaw nagłówek potwierdzenia żądania dla metody ajax exec
     */
    public void setConfirmHeader(String ajaxConfirmHeader) {
        request.setHeader("Ajax-Confirm", ajaxConfirmHeader);
    }

    /*
     Zwraca nazwę zalogowanego użytkownika
     */
    public String getUserName() {
        return session != null && session.user != null ? session.user.username
                : null;
    }

    public static String escapeJS(Object object) {
        return Escape.escape(object);
    }

    public static String escapeURI(String value) {
        return StrUtils.encodeURIComponent(value);
    }

    public static String escapeHTML(String value) {
        return Html.escape(value, true);
    }

    public void setHeader(String name, String value) {
        request.setHeader(name, value);
    }

    public void returnFile(File file) throws IOException {
        request.returnFile(file);
    }

    public void returnFile(CachedData file) throws IOException {
        request.returnFile(file);
    }

    public void returnFile(File file, String eTag) throws IOException {
        request.returnFile(file, eTag);
    }

    public void returnFile(byte[] buffer, String fileName, String mime,
            String eTag)
            throws IOException {
        request.returnFile(buffer, fileName, mime, eTag);
    }

    public void returnFile(byte[] buffer, String fileName)
            throws IOException {
        request.returnFile(buffer, fileName);
    }

    public OutputStream returnFile(String fileName, Integer size, String mime,
            String eTag) throws IOException {
        return request.returnFile(fileName, size, mime, eTag);
    }

    public void redirect(Url location) throws IOException {
        request.redirect(location);
    }

    public void redirect(Class<? extends Controller> page) throws IOException {
        request.redirect(new Url(page));
    }

    public String getCookie(String cookieFieldName) {
        return request.getCookie(cookieFieldName);
    }

}

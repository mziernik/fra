package com.servlet.page_old;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.user.right.RCache;
import com.user.right.RSAdmin;
import com.servlet.controller.Page;
import com.resources.Res;
import com.exceptions.EError;
import com.exceptions.http.Http404FileNotFoundException;
import com.exceptions.http.Http400BadRequestException;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.cache.CachedData;
import com.io.*;
import com.servlet.controller.BaseSession;
import com.context.AppContext;
import com.html.core.styles.*;
import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.Div;
import com.html.js.Eval;
import com.html.modules.LayerDialog;
import com.html.modules.LayerDialog.MessageType;
import com.servlet.interfaces.*;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.ServletOutputStreamEx;
import com.utils.*;
import java.io.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.zip.*;

/**
 * Miłosz Ziernik 2012/12/10
 */
//ToDo Usunąć zbędne metody
@Endpoint(url = {"$$$"}, auth = false, title = "Usługa")
public class Service extends Page {

    @Endpoint()
    public void get() throws Exception {
        returnCacheData(CachedData.get(params.getStr("get")));
    }

    @Endpoint(rights = RSAdmin.class)
    public void gc() {
        System.gc();
    }

    @Endpoint(rights = RCache.class)
    public void clearCache() {
        CachedData.clearCache();
        returnPlainText("clearCache ACK");
    }

    @Endpoint(rights = RSAdmin.class)
    public void invalidateSession() {
        String id = params.getStr("invalidateSession", "");
        BaseSession session = this.session;

        for (BaseSession ses : BaseSession.getSessions())
            if (ses.id != null && !id.isEmpty() && ses.id.equals(id)) {
                session = ses;
                break;
            }
        session.invalidate();
    }

    @Endpoint(url = "delay")
    public void delay() throws InterruptedException {
        Thread.sleep(params.getInt("delay", 30000));
        returnPlainText("");
    }

    @Endpoint(url = "ajaxError", auth = false)
    public void ajaxError() {
        new Log(LogKind.WARNING)
                .tag("Ajax")
                .value(params.getStr("msg", ""))
                .send();
    }

    @Endpoint
    public void preview() throws Exception {
        request.contentDisposition.inline = true;
        returnCacheData(CachedData.get(params.getStr("preview")));
    }

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        if (params.firstStr("").equals("addLog")) {
            Log log = new Log(LogKind.valueOf(params.getStr("knd", "debug")));
            //   log.date = new TDate(params.getStr("dte", ""));
            log.tag(params.getStr("tag", "JavaScript"));
            log.value(params.getStr("val", ""));
            log.send();
            return;
        }

        if (params.exists("jsError")) {
            // -------------------- Błąd JavaScript -------------------------
            String msg = params.getStr("msg", "");
            String file = params.getStr("file", "");
            String line = params.getStr("line", "");
            String[] stack = params.getStr("stack", "").split("\n");
            String tag = params.getStr("tag", "");

            Log log = new Log(LogKind.ERROR);
            log.tag("JavaScript");
            if (!tag.isEmpty())
                log.tag(tag);

            log.callStack.clear();
            log.value(msg);

            for (String s : stack) {
                s = s.trim();
                if (s.startsWith("at "))
                    s = s.substring(3).trim();
//                log.errorStack.add(s);
            }

            log.send();
            return;
        }

        //----------------------------------------------------------------------
        if (params.exists("getZip")) {
            List<String> ids = params.getList("cid");
            boolean contentSize = params.exists("contentSize");

            List<CachedData> cds = new LinkedList<>();
            for (String s : ids)
                cds.add(CachedData.get(s));

            if (cds.isEmpty())
                throw new Http400BadRequestException(this);

            request.setContentType("application/zip");
            request.setHeader("Content-Disposition", "attachment; filename="
                    + StrUtils.convertPolishChars(params.getStr("fileName", "files.zip")));

            OutputStream out = request.getOutputStream();
            ZipOutputStream zos = new ZipOutputStream(out);
            try {
                zos.setLevel(9);
                for (CachedData cd : cds) {
                    ZipEntry ze = new ZipEntry(cd.name);
                    zos.putNextEntry(ze);
                    InputStream in = cd.getInputStream();
                    try {
                        IOUtils.copy(in, zos);
                    } finally {
                        in.close();
                    }
                    zos.closeEntry();
                }
            } finally {
                zos.finish();
                zos.close();
            }

            return;
        }

        String remove = params.getStr("remove", "");
        if (!remove.isEmpty()) {
            CachedData.get(remove).delete();
            returnPlainText("remove ACK " + remove);
            return;
        }

        String checkCache = params.getStr("checkCache", "");
        if (!checkCache.isEmpty()) {
            CachedData cd = CachedData.get(checkCache);
            if (cd != null) {
                returnPlainText("OK" + checkCache);
                return;
            }
            returnPlainText("ERR", 404);;
            return;
        }

        if (params.exists("uploadFile")) {
            returnPlainText("");
            return;
        }

        body.style().fontSize("10pt").fontFamilySans();

        if (params.isEmpty()) {
            redirect(new Url("$status"));
            return;
        }

        throw new Http404FileNotFoundException(this);
    }

    @Endpoint(auth = false)
    public void error() throws Exception {
        String errId = params.getStr("error");
        int code = Utils.strInt(errId, -1);

        // jesli w parametrze przesłano numer błędu, to zwróć jako stronę
        if (code > 0 && code < 1000) {
            returnPlainText("Błąd " + code, code);
            return;
        }

        useAbsolutePaths = true; // aby wyeliminować problemy związane z wyświetlaniem w ramce
        link(Res.utils);

        Throwable e = EError.getException(errId);
        if (e == null)
            throw new Http400BadRequestException(this);

        EError err = new EError(e);

        LayerDialog layer = new LayerDialog(this, MessageType.error);
        layer.caption = err.shortClasses.toString(", ");

        Div div = body.div();
        div.style()
                .marginTop("12px")
                .fontSize("10pt");

        div.textToTags(Div.class, err.shortMessages.toString("\n"));

        if (!AppContext.releaseMode())

            for (Entry<String, String> en : err.details.entrySet()) {

                String name = en.getKey();
                String value = en.getValue();

                if (value == null || value.isEmpty())
                    continue;

                body.br();

                if (name != null && !name.isEmpty())
                    body.div(name + ":").style().marginBottom("4px");

                div = body.div();
                div.style().fontSize("8pt");
                // div.s.textAlign = "center";
                div.textToTags(Div.class, value.trim());
            }

        if (AppContext.devMode && !(e instanceof Error)) {
            body.br();
            Div stack = body.div();
            stack.text("Stos wywołań");
            stack.style().fontSize("8pt")
                    .display(Display.inlineBlock)
                    .color("blue")
                    .textDecoration(TextDecoration.underline)
                    .cursor(Cursor.pointer);

            stack.onClick(new Eval("$id('dStackT').style.display = "
                    + "$id('dStackT').style.display == 'none' ? 'block' : 'none'; "
                    + "layer.change();"));

            div = body.div();
            div.id("dStackT");
            div.style().fontSize("8pt")
                    .display(Display.none)
                    .color("#444")
                    .fontFamily("Courier New");
            div.hr();
            List<Div> lst = div.textToTags(Div.class, EError.getStackTraceStr(e).toString("\n"));

            for (Div t : lst)
                t.text(Char.dot + " " + t.getText());
        }
    }

    @Endpoint
    public static class EmptyHandler extends Page {
        // -------------- ATRAPA -------------------

        @Override
        public void onRequest(HttpRequest http) throws Exception {
            returnPlainText("");
        }
    }

    @Endpoint(url = {"$http"}, auth = false, title = "Żądanie HTTP")
    public static class HttpHeaders extends Page {

        // ToDo: napisac...
        @Override
        public void onRequest(HttpRequest http) throws Exception {
            Ul ul = body.ul();
            //   ul.text("REQUEST");

            //    RequestInfo ri = new RequestInfo(this);
            //    ul.textToListItems(ri.details);
        }
    }
}

package com.servlet.page;

import com.utils.Utils;
import com.utils.Is;
import com.dev.Dev;
import com.utils.text.StrWriter;
import com.servlet.controller.Page;
import com.config.CService;
import com.config.CContent;
import com.exceptions.EError;
import com.mlogger.Log;
import com.context.*;
import com.context.index.Index;
import com.exceptions.EError.SourceCode.SourceCodeLine;
import com.html.core.Html;
import com.html.core.styles.*;
import com.html.core.tag.Body;
import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.*;
import com.html.core.tag.table.*;
import com.html.js.*;
import com.servers.WebAppServer;
import com.servlet.controller.Controller;
import com.servlet.requests.HttpRequest;
import com.sun.management.OperatingSystemMXBean;
import com.utils.Char;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.MemoryUsage;
import java.net.*;
import java.util.*;
import java.util.Map.Entry;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class ErrorPage {

    public void processException(Throwable ex, HttpRequest req, HttpServletRequest request,
            HttpServletResponse response, Controller page) {
        Tr tr;
        Td td;

        boolean release = CService.releaseMode();

        if (page != null && page.http().isLocked())
            return;

        try {
            Html html = new Html();
            Body body = html.body;

            html.head.styles("table")
                    .borderSpacing("0")
                    .marginLeft("32px")
                    .emptyCells("show");

            html.head.styles("body", "table")
                    .fontFamilyMonospace()
                    .lineHeight("1.3em")
                    .fontSize("10pt");

            html.head.styles("table th")
                    .textAlign(TextAlign.left)
                    .fontFamilySans();

            html.head.styles("table td")
                    .padding("0 8px 0 0");

            html.head.styles("table tbody td:first-child", "table tbody div")
                    .display(Display.listItem);

            if (!release)
                html.head.styles("code")
                        .display(Display.block)
                        .padding("6px")
                        .margin("10px")
                        .border("1px solid #aaa")
                        .font("10pt Courier New")
                        .backgroundColor("#ffffee");

            response.setHeader("Content-Disposition", "");

            EError error = new EError(ex);

            int status = response.getStatus();
            if (status <= 200 || status == 500 || status > 600)
                status = error.httpStatus;

            response.setStatus(status);

            body.br();

            Div center = body.div();
            center.style().fontFamilySans().textAlign(TextAlign.center);

            String header = EError.httpStatusToString(error.httpStatus, true);
            header = "Błąd " + status + (header != null
                    ? ", " + header : "");

            if (!release && !error.shortClasses.isEmpty())
                header = "[" + error.shortClasses.toString(", ") + "] " + header;

            center.h3().textToTags(Div.class, header);

            H2 h2 = center.h2();

            h2.textToTags(Div.class, error.shortMessages.toString("\n"));
            h2.style().padding("4px")
                    .color("white")
                    .lineHeight("1.6em")
                    .backgroundColor(error.critical ? "#770000" : "#424D66")
                    .margin("0 20px")
                    .boxShadow("2px 2px 3px #aaaaaa");

            /* if (!release && errors.causeMessage != null
             && !errors.causeMessage.equals(errors.simpletMessage))
             center.h3().text(errors.causeFull);
             */
            center.br();

            if (!release) {
                if (!error.details.isEmpty())
                    center.br();

                for (Entry<String, String> det : error.details.entrySet()) {
                    String key = det.getKey();
                    String val = det.getValue();

                    if (val == null || val.trim().isEmpty())
                        continue;

                    Div div = center.div();
                    div.style().display(Display.inlineBlock);

                    if (key != null && !key.trim().isEmpty())
                        div.h5(key + ":").style()
                                .margin("4px")
                                .marginBottom("0")
                                .color("#666");

                    H4 h4 = div.h4();
                    h4.style()
                            .display(Display.inlineBlock)
                            .textAlign(TextAlign.left)
                            .lineHeight("1.6em")
                            .marginTop("0");

                    h4.textToTags(Div.class, val);

                    center.br();
                }

                html.body.hr();

                Ul ul = body.ul();
                for (EError.EErrorItem it : error)
                    ul.li(it.exception.toString());

                EError.SourceCode sd = error.getSourceCodeLine();

                if (sd != null)
                    try {
                        body.br();
                        if (sd.fileURL != null)

                            body.a(sd.element.getClassName() + ":")
                                    .hrefVoid()
                                    .onClick(Dev.getFileUrl(sd.element) != null
                                            ? new Eval("showFile('" + Dev.getFileUrl(sd.element) + "');") : null
                                    );
                        else
                            body.div(sd.element.getClassName() + ":");
                        Table tt = body.code().table();
                        for (SourceCodeLine sdl : sd.lines) {
                            tr = tt.tbodyTr();
                            tr.td(sdl.number + ". ").style().display(Display.tableCell);
                            tr.td(sdl.value.replace("\t", "  ").replace(" ", Char.nbsp + ""));
                            if (sdl.number == sd.number)
                                tr.style().color("red");
                        }

                        body.br();
                    } catch (Throwable e) {
                        Log.error(e);
                    }

                html.head.script(
                        new Function("expand", "el", "id").body(
                                "var arr = document.querySelectorAll('[data-group=' + id + ']');\n"
                                + "for (var i = 0; i < arr.length; i++)\n"
                                + "  arr[i].style.display = 'table-row';\n"
                                + "el.remove();"
                        ),
                        new Function("showFile", "file").body(
                                "var xmlhttp = new XMLHttpRequest(); "
                                + "xmlhttp.open('POST','" + (page != null
                                        ? page.http().getAbsolutePath("$dev")
                                        : "$dev") + "?showSourceFile',true);"
                                + "xmlhttp.send(file);"
                        )
                );

                try {
                    body.br();

                    center.br();
                    Table tbl = body.table();
                    tr = tbl.theadTr();
                    Th th = tr.th().colspan(3);
                    th.text("Error stack trace:");
                    th.style().paddingBottom("8px");

                    Throwable e = ex;
                    while (e != null) {

                        StackTraceElement[] stack = e.getStackTrace();
                        TBody tbody = tbl.tbody(true);

                        tbody.tr().td(e.getMessage())
                                .colspan(2)
                                .style()
                                .display(Display.tableCell)
                                .color("#c00")
                                .paddingBottom("4px");

                        addTableStackTrace(tbl, stack);

                        e = e.getCause();
                    }

                    //-----------------------------------------------
                    tr = tbl.tbody(true).tr();
                    th = tr.th().colspan(3);
                    th.text("Call stack trace:");
                    th.style()
                            .paddingTop("30px")
                            .paddingBottom("8px");

                    //   tbl.tbody(false).tr().td().style().display(Display.tableCell).height("15px");
                    StackTraceElement[] stack = Thread.currentThread().getStackTrace();

                    TBody tbody = tbl.tbody(true);
                    tbody = tbl.tbody(true);
                    addTableStackTrace(tbl, stack);
                    tbody = tbl.tbody(true);
                    tbody.id("tb2");
                    tbody.style().display(Display.none);
                    /*
                     if (!stack.core.isEmpty()) {
                     addTableStackTrace(tbody, stack.core);
                     tbody = tbl.tbody(true);
                     td = tbody.tr().td();
                     td.colspan(2);
                     td.a("więcej")
                     .hrefVoid()
                     .onClick(new Call("more", "lnk2", "tb2"))
                     .id("lnk2");
                     } */

                    //-----------------------------------------------------------------
                    body.br();
                    tbl = body.table();
                    th = tbl.theadTr().th();
                    th.text("Info:");
                    th.colspan(2);

                    tbl.tbodyTr().setCells("Request:", request.getMethod() + ", " + request.getRequestURL()
                            + (request.getQueryString() != null
                            ? "?" + request.getQueryString() : ""));
                    if (WebAppServer.context != null)
                        tbl.tbodyTr().setCells("Server:", WebAppServer.context.getServerInfo());
                    tbl.tbodyTr().setCells("OS:", System.getProperty("os.name")
                            + " (" + System.getProperty("os.version") + "), "
                            + System.getProperty("os.arch") + ", JRE: " + System.getProperty("java.version"));

                    if (page != null) {
                        if (page.session() != null && page.session().user.username != null)
                            tbl.tbodyTr().setCells("Login:", page.session().user.username);
                        tbl.tbodyTr().setCells("Time:", Long.toString(
                                new Date().getTime() - page.http().createTime.getTime()) + " ms");
                    }

                    Map<String, String[]> parameters = request.getParameterMap();

                    if (!parameters.isEmpty()) {
                        body.br();
                        tbl = body.table();
                        th = tbl.theadTr().th();
                        th.text("Parameters:");
                        th.colspan(2);

                        for (String parameter : parameters.keySet()) {
                            String[] values = parameters.get(parameter);
                            for (int i = 0; i < values.length; i++)
                                tbl.tbodyTr().setCells(parameter
                                        + (values[i] != null && !values[i].isEmpty()
                                        ? ":" : ""), values[i]);
                        }
                    }

                    body.br();
                    tbl = body.table();
                    th = tbl.theadTr().th();
                    th.text("Header:");
                    th.colspan(2);

                    Enumeration<String> headerNames = request.getHeaderNames();

                    while (headerNames.hasMoreElements()) {
                        String s = headerNames.nextElement();
                        if (s.equalsIgnoreCase("cookie"))
                            tbl.tbodyTr().setCells(s + ":", "...");
                        else
                            tbl.tbodyTr().setCells(s + ":", request.getHeader(s));
                    }

                    //----------------------------------------------------------------
                    Cookie[] cookies = request.getCookies();

                    if (cookies != null && cookies.length > 0) {
                        body.br();
                        tbl = body.table();
                        th = tbl.theadTr().th();
                        th.text("Cookies:");
                        th.colspan(2);

                        String sEnc = request.getCharacterEncoding();
                        if (sEnc == null)
                            sEnc = "UTF-8";

                        for (Cookie c : cookies)
                            tbl.tbodyTr().setCells(c.getName() + ":",
                                    URLDecoder.decode(c.getValue(), sEnc));
                    }

                    //----------------------------------------------------------------------
                    if (CContent.errorPageMemoryInfo.value(AppContext.devMode)) {
                        body.br();
                        tbl = body.table();
                        th = tbl.theadTr().th();
                        th.text("Memory:");
                        th.colspan(3);

                        for (MemoryInfo.MemoryInfoItem mi : new MemoryInfo().getAsList())
                            tbl.tbodyTr().setCells(mi.name + ":", "" + mi.value,
                                    "(" + mi.unit + ")");
                    }

//                    //----------------------------------------------------------------
//                    SessionChannel session = request.getSession();
//
//                    Enumeration<String> attribs = session.getAttributeNames();
//                    if (attribs != null && attribs.hasMoreElements()) {
//                        body.br();
//                        tbl = body.table();
//                        th = tbl.theadTr().th();
//                        th.text("Session attributes:");
//                        th.colspan(2);
//
//                        while (attribs.hasMoreElements()) {
//                            String el = attribs.nextElement();
//                            tbl.tbodyTr().setCells(el + ":", session.getAttribute(el).toString());
//                        }
//
//                    }
                } catch (Exception e) {
                    Log.warning(e);
                }

            }

            //------------------------- dodaj dane naglowkowe --------------------------
            //  BPage.addExceptionHeader(ex, request, response);
            response.setContentType("text/html; charset=UTF-8");

            try (StrWriter writer = new StrWriter(req.getWriter(Utils.UTF8))) {
                html.getContent(writer);
            }

        } catch (IOException e) {
            Log.warning(e);
        }
    }

    private static boolean addTableStackTrace(Table table, StackTraceElement[] stackTrace) {
        boolean showFileScript = false;

        TBody tbody = null;

        boolean prev = false;
        int cnt = 0;

        LinkedList<StackTraceElement> list = Utils.asList(stackTrace);
        list.add(null);

        for (StackTraceElement ste : list) {

            boolean pub = ste != null ? Index.svrIdx.files.contains(ste.getClassName().replace(".", "/") + ".class") : prev;

            if (tbody != null && (prev != pub || ste == null)) {
                if (cnt > 3 && !prev) { // tworzenie grupy

                    LinkedList<Tr> rows = tbody.getChildren(Tr.class);
                    rows.pollFirst();
                    rows.pollFirst();
                    rows.pollFirst();

                    String id = Utils.randomId();

                    for (Tr r : rows)
                        r.data("group", id).style().display(Display.none);

                    Td separator = tbody.tr().td().colspan(3);
                    separator.a().text("<więcej>").hrefVoid();
                    separator.onClick(new Call("expand", new Eval("this"), id));

                }

                cnt = 0;
                tbody = null;

            }

            if (ste == null)
                return showFileScript;

            if (tbody == null)
                tbody = table.tbody(true);

            Tr tr = tbody.tr();

            Td td = tr.td();

            if (pub && ste.getFileName() != null && ste.getLineNumber() > 0)

                td.a().text(ste.getClassName() + "." + ste.getMethodName())
                        .hrefVoid()
                        .onClick(Dev.getFileUrl(ste) != null
                                ? new Eval("showFile('" + Dev.getFileUrl(ste) + "');") : null
                        ); //  tr.style().fontWeight(FontWeight.bold);
            else
                td.text(ste.getClassName() + "." + ste.getMethodName()); //  tr.style().color("#888");

            prev = pub;
            ++cnt;

            td = tr.td();
            if (ste.getFileName() != null) {
                td.text("(" + ste.getFileName());
                if (ste.getLineNumber() > 0)
                    td.text(td.getText() + ":" + ste.getLineNumber());
                td.text(td.getText() + ")");
            }
        }
        return showFileScript;
    }

    final static class MemoryInfo {

        public class MemoryInfoItem {

            public final String name;
            public final long value;
            public final String unit;

            public MemoryInfoItem(String name, long value) {
                this.name = name;
                this.value = value;
                this.unit = Utils.formatSize(value);

            }
        }
        public final long freePhysicalMemorySize;
        public final long totalPhysicalMemorySize;
        public final long freeSwapSpaceSize;
        public final long totalSwapSpaceSize;
        public final long heapUsed;
        public final long heapCommitted;
        public final long heapMax;
        public final long heapInit;
        public final long noneHeapUsed;
        public final long noneHeapCommitted;
        public final long noneHeapMax;
        public final long noneHeapInit;
        OperatingSystemMXBean osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);

        public MemoryInfo() {
            freePhysicalMemorySize = osBean.getFreePhysicalMemorySize();
            totalPhysicalMemorySize = osBean.getTotalPhysicalMemorySize();
            freeSwapSpaceSize = osBean.getFreeSwapSpaceSize();
            totalSwapSpaceSize = osBean.getTotalSwapSpaceSize();

            MemoryMXBean mbean = ManagementFactory.getMemoryMXBean();
            MemoryUsage heap = mbean.getHeapMemoryUsage();
            MemoryUsage nheap = mbean.getNonHeapMemoryUsage();

            heapUsed = heap.getUsed();
            heapCommitted = heap.getCommitted();
            heapMax = heap.getMax();
            heapInit = heap.getInit();
            noneHeapUsed = nheap.getUsed();
            noneHeapCommitted = nheap.getCommitted();
            noneHeapMax = nheap.getMax();
            noneHeapInit = nheap.getInit();
        }

        public List<MemoryInfoItem> getAsList() {
            List<MemoryInfoItem> lst = new LinkedList<>();
            lst.add(new MemoryInfoItem("Free Physical Memory Size", freePhysicalMemorySize));
            lst.add(new MemoryInfoItem("Total Physical Memory Size", totalPhysicalMemorySize));
            lst.add(new MemoryInfoItem("Free Swap Space Size", freeSwapSpaceSize));
            lst.add(new MemoryInfoItem("Total Swap Space Size", totalSwapSpaceSize));
            lst.add(new MemoryInfoItem("Heap Used", heapUsed));
            lst.add(new MemoryInfoItem("Heap Committed", heapCommitted));
            lst.add(new MemoryInfoItem("Heap Max", heapMax));
            lst.add(new MemoryInfoItem("Heap Init", heapInit));
            lst.add(new MemoryInfoItem("None Heap Used", noneHeapUsed));
            lst.add(new MemoryInfoItem("None Heap Committed", noneHeapCommitted));
            lst.add(new MemoryInfoItem("None Heap Max", noneHeapMax));
            lst.add(new MemoryInfoItem("None Heap Init", noneHeapInit));
            return lst;
        }

        @Override
        public String toString() {
            return "Free Physical Memory Size: " + freePhysicalMemorySize + " ("
                    + Utils.formatSize(freePhysicalMemorySize) + ")\n"
                    + "Total Physical Memory Size: " + totalPhysicalMemorySize + " ("
                    + Utils.formatSize(totalPhysicalMemorySize) + ")\n"
                    + "Free Swap Space Size: " + freeSwapSpaceSize + " ("
                    + Utils.formatSize(freeSwapSpaceSize) + ")\n"
                    + "Total Swap Space Size: " + totalSwapSpaceSize + " ("
                    + Utils.formatSize(totalSwapSpaceSize) + ")\n"
                    + "Heap Used: " + heapUsed + " (" + Utils.formatSize(heapUsed) + ")\n"
                    + "Heap Committed: " + heapCommitted + " (" + Utils.formatSize(heapCommitted) + ")\n"
                    + "Heap Max: " + heapMax + " (" + Utils.formatSize(heapMax) + ")\n"
                    + "Heap Init: " + heapInit + " (" + Utils.formatSize(heapInit) + ")\n"
                    + "None Heap Used: " + noneHeapUsed + " (" + Utils.formatSize(noneHeapUsed) + ")\n"
                    + "None Heap Committed: " + noneHeapCommitted + " (" + Utils.formatSize(noneHeapCommitted) + ")\n"
                    + "None Heap Max: " + noneHeapMax + " (" + Utils.formatSize(noneHeapMax) + ")\n"
                    + "None Heap Init: " + noneHeapInit + " (" + Utils.formatSize(noneHeapInit) + ")\n";
        }
    }

}

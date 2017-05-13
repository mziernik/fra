package com.mlogger;

import com.utils.Utils;
import com.utils.Is;
import com.utils.console.TConsole;
import com.utils.text.StrWriter;
import com.servlet.controller.BaseSession;
import com.utils.date.TDate;
import com.mlogger.interfaces.LogException;
import com.mlogger.interfaces.ILogEventsHandler;
import com.context.AppConfig;
import com.context.AppContext;
import com.servlet.controller.*;
import com.servlet.handlers.*;
import com.servlet.requests.HttpRequest;
import com.utils.collections.Params.Param;
import com.utils.collections.Strings;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.net.*;
import java.util.*;
import java.util.logging.Handler;
import java.util.logging.Level;

import static com.utils.Utils.*;

import com.context.Environment;
import javax.servlet.http.Cookie;
import org.slf4j.impl.SLF4JLogger;

/**
 * Miłosz Ziernik 2012/11/22
 */
public class ServletLog implements ILogEventsHandler {

    public static boolean logResourceRequest = false;

    public ServletLog() {
        AppContext.logger.setLevel(Level.ALL);
        // wylacz monitorowanie zasobow

    }

//    @Override
//    public boolean onBeforeAddStatistic(Statictic stat) {
//        return CDebug.statisticsEnabled.value(AppContext.devMode);
//    }
    @Override
    public void onAfterCreate(Log log) {

        log.extra.put("$req", Thread.currentThread());

        if (log.source.isEmpty())
            log.source.set(AppConfig.getServiceTitle());

        log.hostName.set(Environment.hostname);

        if (AppContext.serviceStatus != null && AppContext.serviceStatus.version != null)
            log.version.set(AppContext.serviceStatus.version
                    + "." + AppContext.serviceStatus.build);

        HttpRequest req = HttpRequest.getInstance();
        if (req != null) {
            log.address(req.request.getRemoteHost());

            String xForwardedFor = req.getHeader("x-forwarded-for");
            if (xForwardedFor != null && !xForwardedFor.isEmpty())
                log.address(xForwardedFor);

            log.userAgent.set(req.getHeader("User-Agent"));
            log.url(req.url.toString());
            log.request.set(req.id);

            if (req.session != null) {
                log.session.set(req.session.id);
                log.user.set(req.session.user != null ? req.session.user.username : null);
            }

        }
    }

    @Override
    public boolean onBeforeSend(Handler handler, Log log) {
        try {
            Controller page = Controller.getInstance((Thread) log.extra.get("$thread$"), false);
            if (page != null && page.endpoint().disableLogs())
                return false;
        } catch (Throwable e) {
            return true;
        }
        return true;
    }

    @Override
    public void onException(LogException e) {
        if (AppContext.devMode)
            TConsole.printErr(e);
    }

    @Override
    public void onInfo(Handler handler, LogElement log, String tag, String info) {
    }

    /*
     public final static class RequestInfo {

     public final String details;
     public final String header;
     public final String content;
     public int size;

     public RequestInfo(BPage page) {
     String h = "";
     StringWriter sw = new StringWriter();
     String ct = null;

     try {
     h = page.request.getMethod() + ", " + page.request.getServletPath();

     if (page.request.getQueryString() != null)
     h += "?" + page.request.getQueryString();

     size = page.request.getContentLength();

     //------------------------------------------------------------------
     sw.append("REQUEST, ").append(page.request.getMethod());
     if (page.request.isSecure())
     sw.append(", SSL");

     sw.append(", thread ").append(Long.toString(Thread.currentThread().getId())).append(":\n");

     sw.append("URI: ").append(page.request.getRequestURL());
     if (page.request.getQueryString() != null)
     sw.append("?").append(page.request.getQueryString());
     sw.append("\n");

     sw.append("Parameters: \n");

     for (RequestParam param : page.params) {
     String s = param.name;
     if (s == null)
     continue;
     s = s.toLowerCase();
     if (s.equals("password") || s.equals("pass") || s.equals("haslo"))
     sw.append(" ").append(param.name).append(": *************\n");
     else {
     sw.append(" ").append(param.name).append(": ");

     // jesli rozmiar postu jest zbyt duzy to przytnij
     if (param.value.length() > 300) {
     sw.append(param.value.substring(0, 300)).append(" [...] rozmiar: ");
     sw.append(Utils.formatFileSize(param.value.length()));
     } else
     sw.append(param.value);
     sw.append("\n");
     }
     }

     sw.append("Header: \n");

     Enumeration<String> headerNames = page.request.getHeaderNames();

     while (headerNames.hasMoreElements()) {
     String s = headerNames.nextElement();
     sw.append(" ").append(s).append(": ").append(page.getHeader(s)).append("\n");
     }

     } catch (Exception e) {
     }

     try {
     if (page.request.inputData != null) {
     int len = page.request.inputData.size();
     ct = new String(page.request.inputData.toByteArray(), Utils.UTF8);
     StringBuilder sb = new StringBuilder();
     for (int i = 0; i < ct.length(); i++) {
     char c = ct.charAt(i);
     sb.append(c < 32 && c != 13 && c != 10 ? '.' : c);
     }

     ct = sb.toString();
     if (len > 1000)
     ct += '…';
     }
     } catch (Exception e) {
     }

     content = ct;
     header = h;
     details = sw.toString();
     }
     }

     public final static class ResponseInfo {

     public final String details;
     public final String header;
     public final String content;
     public long size;

     public ResponseInfo(BPage page) {
     String h = "";
     StringWriter sw = new StringWriter();
     String ct = null;

     try {
     h = "" + page.response.getStatus();
     if (page.response.getContentType() != null) {
     h += ", " + page.response.getContentType();
     if (h.indexOf(";") > 0)
     h = h.substring(0, h.lastIndexOf(";"));
     }

     String cd = page.response.getHeader("Content-Disposition");
     if (cd != null && cd.indexOf("filename") > 0) {
     cd = cd.substring(cd.indexOf("filename"), cd.length());
     cd = cd.substring(cd.indexOf("=") + 1, cd.length());

     if (cd.startsWith("\""))
     cd = cd.substring(1, cd.length());

     if (cd.endsWith("\""))
     cd = cd.substring(0, cd.length() - 1);

     cd = URLDecoder.decode(cd, Utils.UTF8);

     cd = Utils.cutLongName(cd, 40, true);
     h += ", \"" + cd + "\"";
     }

     if (page.response.outputStream != null) {
     size = page.response.outputStream.length;
     if (page.response.outputStream.aborted)
     h += " (przerwany)";
     }

     //--------------------------------------------------------------
     sw.append("RESPONSE, thread ");
     sw.append(Long.toString(Thread.currentThread().getId())).append(":\n");
     sw.append(" Status: ").append(Integer.toString(page.response.getStatus())).append("\n");
     List<String> lst = new LinkedList<>();

     Collection<String> headerNames = page.response.getHeaderNames();
     String[] headers = new String[headerNames.size()];
     headerNames.toArray(headers);
     for (int i = 0; i < headers.length; i++) {
     boolean f = false;

     for (int j = 0; j < lst.size(); j++)
     if (lst.get(j).equals(headers[i])) {
     f = true;
     break;
     }
     if (f)
     continue;

     lst.add(headers[i]);

     Collection<String> values = page.response.getHeaders(headers[i]);
     Iterator<String> iterator = values.iterator();
     while (iterator.hasNext())
     sw.append(" ").append(headers[i]).append(": ").append(iterator.next()).append("\n");
     }

     try {
     if (page.response.outputStream != null) {
     int len = (int) page.response.outputStream.length;
     if (len > page.response.outputStream.content.length)
     len = page.response.outputStream.content.length;

     ct = new String(Arrays.copyOf(page.response.outputStream.content, len), Utils.UTF8);
     StringBuilder sb = new StringBuilder();
     for (int i = 0; i < ct.length(); i++) {
     char c = ct.charAt(i);
     sb.append((c < 32 || c > 127) && c != 13 && c != 10 ? '.' : c);
     }

     ct = sb.toString();
     if (page.response.outputStream.content.length
     < page.response.outputStream.length)
     ct += '…';
     }
     } catch (Exception e) {
     }

     } catch (Exception e) {
     }

     content = ct;
     header = h;
     details = sw.toString();

     }
     }
     */
    public static Log requestInfo(HttpRequest http) {

        Log log = new Log(LogKind.REQUEST);
        log.tag("Request", http.getMethod().name());

        if (http.isAjaxRequest)
            log.tag("Ajax");

        if (http.isSecure())
            log.tag("SSL");

        {
            String value = http.url.toString();

            String sRange = http.getHeader("range");
            if (sRange != null && sRange.contains("bytes")) {

                sRange = sRange.replace(" ", "");
                sRange = sRange.substring(sRange.indexOf("=") + 1);

                String[] split = sRange.split("\\-", -1);
                if (split.length == 2) {
                    Long min = Utils.strLong(split[0], null);
                    Long max = Utils.strLong(split[1], null);
                    value += ", range " + (min != null ? Utils.formatSize(min) : "")
                            + " - " + (max != null ? Utils.formatSize(max) : "");
                }

            }

            log.value(value);
        }
        if (http.controller != null)
            log.attribute("Kontroler", http.controller.getClass().getName());

        Strings comment = new Strings().nonEmpty(true);

        if (http.request.getContentType() != null) {
            String h = http.request.getContentType();
            if (h.indexOf(";") > 0)
                h = h.substring(0, h.lastIndexOf(";"));
            comment.add(h);
        }

        comment.add(http.request.getHeader("Content-Encoding"));

        if (http.request.getContentLength() > 0)
            comment.add(Utils.formatSize(http.request.getContentLength()));

        log.comment(comment.toString(", "));

        String par = "Parameters";

        for (Param param : http.params) {
            String s = param.name;
            String value = Utils.coalesce(Utils.toString(param.value), "");
            if (s == null)
                continue;
            if (s.equalsIgnoreCase("cookie"))
                log.attribute(par, s, "...");
            else if (s.contains("password") || s.contains("pass") || s.contains("haslo"))
                log.attribute(par, s, value.replaceAll(".", "*"));
            else // jesli rozmiar postu jest zbyt duzy to przytnij
            if (value.length() > 200)
                log.attribute(par, s, value.substring(0, 197) + "[…]");
            else
                log.attribute(par, s, value);
        }

        Enumeration<String> headerNames = http.request.getHeaderNames();
        while (headerNames.hasMoreElements()) {
            String s = headerNames.nextElement();
            log.attribute("Header", s, http.getHeader(s));
        }

        Cookie[] cookies = http.request.getCookies();
        if (cookies != null)
            for (Cookie ck : cookies)
                if (ck != null)
                    log.attribute("Cookies", ck.getName(), ck.getValue());

        Enumeration<String> attrs = http.request.getAttributeNames();
        while (attrs.hasMoreElements()) {
            String s = attrs.nextElement();
            Object val = http.getAttribute(s);
            log.attribute("Attribute", s, val);
        }

        return log;
    }

    public static Log responseInfo(HttpRequest http) {

        Log log = new Log(LogKind.REQUEST);
        log.tag("Response", "" + http.response.getStatus());
        Strings comment = new Strings().nonEmpty(true);

        if (http.isCorsRequest)
            log.tag("CORS");

        if (http.controller != null)
            log.attribute("Kontroler", http.controller.getClass().getName());

        long size = 0;

        try {
            if (http.response.getContentType() != null) {
                String ct = http.response.getContentType();
                if (ct.indexOf(";") > 0)
                    ct = ct.substring(0, ct.lastIndexOf(";"));
                comment.add(ct);
            }

            comment.add(http.response.getHeader("Content-Encoding"));

            log.value(http.url);

            String cd = Utils.coalesce(http.getHeader("Content-Disposition"), "");
            if (cd.contains("filename")) {
                cd = cd.replace("filename*=utf-8''", "filename=");
                cd = cd.substring(cd.indexOf("filename"), cd.length());
                cd = cd.substring(cd.indexOf("=") + 1, cd.length());
                if (cd.startsWith("\""))
                    cd = cd.substring(1, cd.length());
                if (cd.endsWith("\""))
                    cd = cd.substring(0, cd.length() - 1);
                try {
                    cd = URLDecoder.decode(cd, "UTF-8");
                } catch (Exception e) {
                }
                comment.add(cd + "\"");
            }

            if (http.outputStream != null) {
                size = http.outputStream.length;
                if (http.outputStream.aborted)
                    comment.add("przerwany");
            }
            comment.add(Utils.formatSize(size));
            comment.add(new Interval(System.nanoTime() - http.createTS,
                    Unit.NANOSECONDS).toString());

            List<String> lst = new LinkedList<>();

            Collection<String> headerNames = http.response.getHeaderNames();
            String[] headers = new String[headerNames.size()];
            headerNames.toArray(headers);
            for (int i = 0; i < headers.length; i++) {
                boolean f = false;

                for (int j = 0; j < lst.size(); j++)
                    if (lst.get(j).equals(headers[i])) {
                        f = true;
                        break;
                    }
                if (f)
                    continue;

                lst.add(headers[i]);

                Collection<String> values = http.response.getHeaders(headers[i]);
                Iterator<String> iterator = values.iterator();
                while (iterator.hasNext())
                    log.attribute("Header", headers[i], iterator.next());
            }

            try {
                if (http.outputStream != null) {
                    int len = (int) http.outputStream.length;
                    if (len > http.outputStream.content.size())
                        len = http.outputStream.content.size();

                    String ct = new String(Arrays.copyOf(http.outputStream.content.toByteArray(), len), Utils.UTF8);
                    StrWriter sb = new StrWriter();
                    for (int i = 0; i < ct.length(); i++) {
                        char c = ct.charAt(i);
                        sb.append((c < 32 || c > 127) && c != 13 && c != 10
                                ? '.' : c);
                    }

                    ct = sb.toString();
                    if (http.outputStream.content.size() < http.outputStream.length)
                        ct += '…';

                    log.data("Treść", ct);
                }
            } catch (Exception e) {
            }

        } catch (Exception e) {
            Log.warning(e);
        }

        log.comment(comment.toString(", "));

        return log;
    }

    @Override
    public void onError(Log log, Throwable e) {

    }

    @Override
    public boolean onSLF4JLog(SLF4JLogger slf, Log log) {
        return true;
    }
}

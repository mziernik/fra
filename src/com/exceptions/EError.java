package com.exceptions;

import com.config.CHttp;
import com.utils.Utils;
import com.utils.Is;
import com.utils.console.TConsole;
import com.utils.text.StrWriter;
import com.exceptions.http.NotFoundError;
import com.config.CService;
import com.context.*;
import com.exceptions.EError.EErrorItem;
import com.exceptions.intf.IDetailedException;
import com.exceptions.intf.IHttpException;
import com.html.core.Html;
import com.html.core.styles.Selector;
import com.html.core.styles.TextAlign;
import com.html.core.tag.Body;
import com.html.core.tag.formatting.Code;
import com.html.core.tag.list.Li;
import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.io.IOUtils;
import com.mlogger.Log;
import com.mlogger.LogElement;
import com.resources.Res;
import com.resources.core.ResData;
import com.resources.core.Resources;
import com.utils.Str;
import com.servlet.Handlers;
import com.servlet.requests.HttpRequest;
import com.utils.*;
import com.utils.collections.*;
import com.utils.date.TDate;
import com.utils.reflections.TClass;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.*;
import java.util.*;

/**
 * Klasa zawierające różne metody do obsługi błędów
 *
 * @author milosz
 */
public class EError implements Iterable<EErrorItem> {

    private final static ShiftMap<Throwable, Map<String, String>> detailsMap
            = new ShiftMap<>(100, 1000 * 60 * 60 * 24);

    private final static ShiftMap<String, Throwable> allExceptions
            = new ShiftMap<>(100, 1000 * 60 * 60 * 24);

    public static ErrorMessage format(Throwable e) {
        return Handlers.errors.getInstance().getMessage(e);
    }

    public class EErrorItem {

        public final Throwable exception;
        public final String message;
        public final String messageSimple;
        public final boolean critical;
        public final Map<String, String> details = new LinkedHashMap<>();
        public final String classFull;
        public final String classSimple;
        public final String classShort;

        public EErrorItem(Throwable ex) {

            String msg = Utils.coalesce(ex.getLocalizedMessage(), "");

            this.exception = ex;
            this.message = msg;

            Throwable cause = ex.getCause();

            if (cause != null && msg.equals(cause.toString()))
                msg = "";

            String pckg = exception.getClass().getPackage().getName();

            this.critical = ex instanceof LinkageError || !(ex instanceof Error);

            this.classFull = ex.getClass().getName();
            String s = this.classSimple = ex.getClass().getSimpleName();

            if (s.endsWith("Exception") && !s.startsWith("Exception"))
                s = s.substring(0, s.length() - "Exception".length());

            if (s.endsWith("Error") && !s.startsWith("Error"))
                s = s.substring(0, s.length() - "Error".length());

            this.classShort = s;

            this.messageSimple = msg;

            details.putAll(getDetails(exception));

            if (ex instanceof IDetailedException) {
                Map<String, String> map = new LinkedHashMap<>();
                ((IDetailedException) ex).getDetails(map);
                details.putAll(map);

            }
        }

    }

    /**
     * ToDo lista metod danej klasy
     */
    public final EErrorItem[] items;
    public final Throwable exception;
    public final int httpStatus;
    public final boolean critical;
    public final Strings shortClasses = new Strings().unique(true); // NullPointer
    public final Strings simpleClasses = new Strings().unique(true);
    public final Strings shortMessages = new Strings().separator("\n");
    public final Map<String, String> details = new LinkedHashMap<>();

    public EError(Throwable exception) {
        this.exception = Objects.requireNonNull(exception, "EError.exception");

        Throwable ex = exception;
        int size = 0;
        List<EErrorItem> list = new LinkedList<>();

        while (ex != null && ++size < 100) {
            EErrorItem item = new EErrorItem(ex);
            list.add(0, item);
            if (!item.messageSimple.isEmpty() && !item.messageSimple.equals(shortMessages.last()))
                shortMessages.add(item.messageSimple);

            if (item.classShort != null && !item.classShort.equals(shortClasses.last()))
                shortClasses.insert(item.classShort);

            if (item.classSimple != null && !item.classSimple.equals(simpleClasses.last()))
                simpleClasses.insert(item.classSimple);

            ex = ex.getCause();
            details.putAll(item.details);
        }

        items = list.toArray(new EErrorItem[0]);

        if (shortMessages.isEmpty())
            shortMessages.insert(shortClasses.first());

        if (shortMessages.isEmpty())
            shortMessages.add(first().classSimple);

        this.httpStatus = getHttpStatus(exception);

        boolean critical = true;
        for (EErrorItem it : items)
            if (!(it.exception instanceof InvocationTargetException)
                    && !(it.exception instanceof ThrowableException))
                critical = it.critical;

        this.critical = critical;
    }

    public static <E extends Throwable> E addDetails(E exception, String name, String details) {
        synchronized (detailsMap) {
            Map<String, String> map = detailsMap.get(exception);
            if (map == null)
                map = new LinkedHashMap<>();

            map.put(name, details);
            detailsMap.put(exception, map);
        }
        return exception;
    }

    public Map<String, String> getDetails(Throwable exception) {
        Map<String, String> map;
        synchronized (detailsMap) {
            map = detailsMap.get(exception);
        }
        if (map == null)
            map = new LinkedHashMap<>();
        return map;
    }

    public static Throwable getNonReflectException(Throwable exception) {
        if (exception == null)
            return null;
        if ((exception instanceof InvocationTargetException
                || exception instanceof ThrowableException)
                && exception.getCause() != null)
            return exception.getCause();

        return exception;
    }

    public static int getHttpStatus(Throwable exception) {

        Throwable ex = getNonReflectException(exception);

        int httpStatus = 500;

        while (httpStatus == 500 && ex != null) {

            if ((ex instanceof ConnectException)
                    || (ex instanceof SocketTimeoutException)
                    || (ex.getClass().toString().toLowerCase().contains("timeout")))
                httpStatus = 504;

            if (ex instanceof AuthenticationException)
                httpStatus = 401;

            if (ex instanceof ForbiddenError || exception instanceof AccessDeniedException)
                httpStatus = 403;

            if (ex instanceof FileNotFoundException || exception instanceof NotFoundError)
                httpStatus = 404;

            if (ex instanceof UnsupportedOperationException)
                httpStatus = 501;

            if (ex.getClass() == Error.class
                    || (ex instanceof Error
                    && !ex.getClass().getPackage().getName().equals("java.lang")))
                httpStatus = 400;

            if (ex instanceof IHttpException)
                httpStatus = ((IHttpException) ex).getHttpStatus();

            ex = ex.getCause();
        }

        return Handlers.errors.getInstance().onGetHttpStatus(ex, httpStatus);
    }

    public EErrorItem first() {
        return items.length > 0 ? items[items.length - 1] : null;
    }

    public EErrorItem last() {
        return items.length > 0 ? items[0] : null;
    }

    @Override
    public String toString() {
        return toString(false);
    }

    public String toString(boolean inclClasses) {
        return inclClasses ? "[" + shortClasses.toString(", ") + "] "
                + shortMessages.toString(": ")
                : shortMessages.toString(": ");
    }

    public static String toString(Throwable ex) {
        return toString(ex, false);
    }

    public static String toString(Throwable ex, boolean inclClasses) {
        if (ex == null)
            return "";

        return new EError(ex).toString(inclClasses);
    }

    public static String httpStatusToString(int status, boolean polish) {

        switch (status) {
            case 400:
                return polish ? "Błędne żądanie" : "Bad Request";
            case 401:
                return polish ? "Brak autoryzacji" : "Unauthorized";
            case 402:
                return polish ? "Wymagana opłata" : "Payment Required";
            case 403:
                return polish ? "Dostęp zabroniony" : "Forbidden ";
            case 404:
                return polish ? "Nie znaleziono" : "Not Found";
            case 405:
                return polish ? "Metoda nie dozwolona" : "Method Not Allowed";
            case 406:
                return polish ? "Brak akceptacji" : "Not Acceptable";
            case 407:
                return polish ? "Wymagana autoryzacja proxy"
                        : "Proxy Authentication Required";
            case 408:
                return polish ? "Zapytanie przekroczyło czas oczekiwania"
                        : "Request Time-out";
            case 409:
                return polish ? "Konflikt" : "Conflict";
            case 410:
                return polish ? "Zasób niedostępny" : "Gone";
            case 411:
                return polish ? "Wymagana długość" : "Length Required";
            case 412:
                return polish ? "Nie spełniono warunku wstępnego"
                        : "Precondition Failed";
            case 413:
                return polish ? "Zbyt duże żądanie" : "Request Entity Too Large";
            case 414:
                return polish ? "Adres URI zapytania jest zbyt długi"
                        : "Request-URI Too Large";
            case 415:
                return polish ? "Niewspierany typ medium"
                        : "Unsupported Media Type";
            case 416:
                return polish ? "Żądany zakres nie obsługiwany"
                        : "Requested range not satisfiable";
            case 417:
                return polish ? "Niepowodzenie oczekiwania"
                        : "Expectation Failed";
            //Server errors - po stronie serwera
            case 500:
                return polish ? "Wewnętrzny błąd serwera"
                        : "Internal Server Error";
            case 501:
                return polish ? "Niezaimplementowane" : "Not Implemented";
            case 502:
                return polish ? "Błąd bramy" : "Bad Gateway";
            case 503:
                return polish ? "Usługa niedostępna" : "Service Unavailable";
            case 504:
                return polish ? "Przekroczony czas oczekiwania bramy"
                        : "Gateway Time-out";
            case 505:
                return polish ? "Brak wsparcia dla tej wersji HTTP"
                        : "HTTP Version not supported";
            case 509:
                return polish ? "Przekroczono limit transferu"
                        : "Bandwidth Limit Exceeded";
        }
        return null;
    }

    public Strings getStackTraceStr() {
        return stackTraceToString(getStackTrace(exception));
    }

    public abstract static interface IExceptionHandler {

        public abstract void onException(Exception ex);
    }

    public static String exceptionToStr(Throwable e) {
        return exceptionToStr(e, false);
    }

    @Override
    public Iterator<EErrorItem> iterator() {
        return Arrays.asList(items).iterator();
    }

    public static String getClassName(Throwable ex) {
        if (ex == null)
            return "";

        List<String> names = new LinkedList<>();

        Throwable e = ex;
        while (e != null) {
            String name = e.getClass().getSimpleName();
            if (!names.contains(name))
                names.add(0, name);
            e = e.getCause();
        }
        return new Strings(names).toString(", ");
    }

    public static String exceptionToStr(Throwable ex, boolean addStackTrace) {
        if (ex == null)
            return "null";

        StringWriter sw = new StringWriter();
        sw.append("[").append(getClassName(ex));

        sw.write("] ");
        sw.write(toString(ex));

        if (addStackTrace) {
            sw.write("\n\n");
            ex.printStackTrace(new PrintWriter(sw));
        }

        return sw.toString().trim();
    }

    private static List<StackTraceElement> getTrimmedStackTrace(StackTraceElement[] stackTrace) {
        List<StackTraceElement> lst = new LinkedList<>();

        final String[] exclude = {
            "javax.servlet.http.HttpServlet",
            "org.apache.catalina.core.ApplicationFilterChain",
            "org.apache.catalina.session.StandardSession",
            "org.apache.catalina.core.StandardContext"};

        for (StackTraceElement ste : stackTrace) {
            boolean cut = false;
            for (String s : exclude)
                if (s.equals(ste.getClassName())) {
                    cut = true;
                    break;
                }
            if (cut)
                break;
            lst.add(ste);
        }

        if (lst.isEmpty())
            lst.addAll(Arrays.asList(stackTrace));

        return lst;
    }

    public static Strings stackTraceToString(StackTraceElement[] stackTrace) {
        return stackTraceToString(Arrays.asList(stackTrace));
    }

    public static Strings stackTraceToString(Collection<StackTraceElement> stackTrace) {
        Strings result = new Strings();
        result.separator("\n");

        for (StackTraceElement ste : stackTrace) {
            StrWriter sb = new StrWriter();
            if (ste == null) {
                sb.append("[...]\n");
                continue;
            }
            sb.append(ste.getClassName()).append(".").append(ste.getMethodName());

            if (ste.getFileName() != null && ste.getLineNumber() > 0) {
                sb.append(" (").append(ste.getFileName()).append(":");
                sb.append(ste.getLineNumber()).append(")");
            }
            result.add(sb.toString());
        }
        return result;
    }

    public static List<StackTraceElement> getStackTrace(int level) {
        if (level < -2)
            level = -2;
        List<StackTraceElement> st
                = getTrimmedStackTrace(Thread.currentThread().getStackTrace());
        if (st.size() > 2)
            for (int i = 0; i < level + 2; i++)
                st.remove(0);
        return st;
    }

    public static List<StackTraceElement> getStackTrace(Throwable e) {
        return getTrimmedStackTrace(e.getStackTrace());
    }

    public static Strings getStackTraceStr(Throwable ex) {

        Throwable e = ex;

        List<StackTraceElement> stackTrace = new LinkedList<>();
        while (e != null) {
            if (!stackTrace.isEmpty())
                stackTrace.add(null);
            stackTrace.addAll(getStackTrace(e));
            e = e.getCause();
        }
        return stackTraceToString(stackTrace);
    }

    public static boolean instanceOf(Throwable e, Class<? extends Throwable>... classes) {

        if (classes == null)
            return false;

        Throwable err = e;

        while (err != null) {
            for (Class<? extends Throwable> cls : classes)
                if (new TClass(cls).instanceOf(err.getClass()))
                    return true;

            err = err.getCause();
        }

        return false;
    }

    public static SocketException processSocketException(SocketException e, SocketAddress address) {
        if (e instanceof BindException)
            return new BindException(new Str(e.getMessage())
                    .removeSufix(": JVM_Bind")
                    .sufix(": " + address.toString())
                    .toString());
        return e;
    }

    public static class SourceCode {

        public final String line;
        public final SourceCodeLine[] lines;
        public final int number;
        public final String fileName;
        public final URL fileURL;
        public final String method;
        public final StackTraceElement element;

        public class SourceCodeLine {

            public final int number;
            public final String value;

            public SourceCodeLine(int number, String value) {
                this.number = number;
                this.value = value;
            }

        }

        private SourceCode(StackTraceElement ste, String[] lines, URL fileURL) {
            this.element = ste;
            this.number = ste.getLineNumber();
            this.fileName = ste.getFileName();
            this.method = ste.getMethodName();
            this.fileURL = fileURL;

            int nr = ste.getLineNumber() - 1;

            List<SourceCodeLine> lst = new LinkedList<>();

            for (int i = nr - 4; i < nr + 4; i++) {
                if (nr < 0 || nr >= lines.length)
                    continue;

                if (i >= lines.length)
                    break;

                lst.add(new SourceCodeLine(i + 1, lines[i]));
            }

            this.lines = lst.toArray(new SourceCodeLine[0]);
            this.line = lines[nr];
        }

    }

    private static final ShiftMap<String, SourceCode> sourceCodeCache = new ShiftMap(100, 24 * 60 * 60 * 1000);

    public SourceCode getSourceCodeLine() {

        try {
            Throwable ex = last() != null ? last().exception : null;

            if (ex == null)
                return null;

            StackTraceElement ste = Handlers.errors.getInstance().getExceptionReason(ex);

            if (ste == null)
                return null;

            SourceCode sc;
            String key = ste.getFileName() + ":" + ste.getLineNumber();
            synchronized (sourceCodeCache) {
                sc = sourceCodeCache.get(key);
            }
            if (sc != null)
                return sc;

            String cls = ste.getClassName();
            if (cls.contains("."))
                cls = cls.substring(0, cls.lastIndexOf("."));

            cls = cls.replace(".", "/");
            String fileName = "/" + cls + "/" + ste.getFileName();

            byte[] data = null;
            URL fileURL = null;

            if (!AppContext.sourcesPath.isEmpty()) {
                File file = AppContext.sourcesPath.getFile(fileName);
                if (file.exists() && file.isFile()) {
                    data = IOUtils.read(file);
                    fileURL = file.toURI().toURL();
                }
            }

            if (data == null) {
                ResData res = Resources.get(fileName);
                if (res != null) {
                    fileURL = res.getSourceURL();
                    data = res.getData();
                }
            }

            if (data == null)
                return null;

            String[] lines = new String(data).split("\\n");

            if (lines.length > ste.getLineNumber() - 1) {
                sc = new SourceCode(ste, lines, fileURL);

                synchronized (sourceCodeCache) {
                    sourceCodeCache.put(key, sc);
                }

                return sc;

            }

        } catch (Throwable e) {
            TConsole.printErr(e);
        }

        return null;

    }

    public static String addException(Throwable e) {
        String id = Utils.randomId(10);
        synchronized (allExceptions) {
            allExceptions.put(id, e);
        }
        return id;
    }

    public static Throwable getException(String key) {
        synchronized (allExceptions) {
            return allExceptions.get(key);
        }
    }

    public Html buildHTML(Log log, boolean b) {

        Html html = new Html();

        Body body = html.body;
        body.style().padding("16px");

        body.styles("*").fontFamily("Verdana, Tahoma, Arial")
                .fontSize("10pt")
                .color("#000000")
                .backgroundColor("#ffffff");

        body.styles(".tbl-attribs td").padding("3px 6px");

        body.h2(toString(true))
                .style()
                .textAlign(TextAlign.center)
                .fontSize("11pt")
                .color("#880000")
                .borderTop("1px solid #cccccc")
                .borderBottom("1px solid #cccccc")
                .padding("8px")
                .marginBottom("20px")
                .backgroundColor("#f7f7f7");;

        HttpRequest http = HttpRequest.getInstance();

        {
            Table tbl = body.table();
            tbl.cls("tbl-attribs");

            tbl.tbodyTr().setCells("Data:", new TDate().toString(false));
            tbl.tbodyTr().setCells("Wersja:", log.version.value());
            tbl.tbodyTr().setCells("Profil:", CService.mode.value().displayName);
            tbl.tbodyTr().setCells("Host:", Environment.hostname);
            Tr tr = tbl.tbodyTr();
            tr.td("URL:");
            tr.td().a(CHttp.url.value()).href(new Url(CHttp.url.value()));
            tbl.tbodyTr().setCells("Proces:", log.processId.value());
            tbl.tbodyTr().setCells("Wątek:", log.threadId.value() + ", " + log.threadName.value());
            tbl.tbodyTr().setCells("Rodzaj:", log.kind.value());

            if (http != null) {
                tr = tbl.tbodyTr();
                tr.td(Char.nbsp).colspan(2);

                tbl.tbodyTr().setCells("Adres:", new Strings(log.address.value()).toString(", "));
                tbl.tbodyTr().setCells("URL:", http.url);
                tbl.tbodyTr().setCells("Użytkownik:", http.session.user != null ? http.session.user.username : null);
                tbl.tbodyTr().setCells("UserAgent:", http.userAgent);
            }

        }

        //----------------------------------------------------------------------
        body.hr().style()
                .marginTop("25px")
                .height("1px")
                .color("#999");

        for (List<String> ll : log.errorStack) {
            Div div = body.div();
            div.cls("est");

            div.br();

            LinkedList<String> list = new LinkedList<>(ll);
            div.b(list.pollFirst());

            Ul ul = div.ul();
            for (String s : list) {

                Li li = ul.li();
                li.style()
                        .color("#666")
                        .padding("2px")
                        .fontSize("8pt")
                        .fontFamilyMonospace();

                if (s.startsWith("*") || s.startsWith("+")) {
                    s = s.substring(1).trim();
                    li.text(s).style().color("#000");
                } else
                    li.text(s);

            }

        }

        if (!log.data.isEmpty())

            for (LogElement.DataObj en : log.data.value()) {
                body.br();
                body.h5(en.name);
                body.pre(en.value).style().fontSize("9pt").color("#555");
                body.br();
            }

        //----------------------------------------------------------------------
        if (!log.sourceCode.isEmpty()) {
            body.hr().style()
                    .marginTop("25px")
                    .height("1px")
                    .color("#999");

            Div div = body.div();
            div.style().paddingTop("10px");

            SourceCode src = log.sourceCode.value();

            div.div(src.fileName + ":" + src.number);
            Table tbl = div.table();
            tbl.style()
                    .borderSpacing("0")
                    .marginTop("10px")
                    .border("1px solid #999")
                    .width("100%");

            for (SourceCode.SourceCodeLine line : src.lines) {
                Tr tr = tbl.tbodyTr();
                tr.td(line.number + ".")
                        .style()
                        .backgroundColor("#eee")
                        .textAlign(TextAlign.center)
                        .width("30px")
                        .borderRight("1px solid #999");

                Selector style = tr.td().pre(line.value)
                        .style()
                        .color("#333")
                        .padding("0")
                        .margin("0")
                        .fontFamilyMonospace();
                if (line.number == src.number)
                    style.borderTop("1px solid #cc8888")
                            .borderBottom("1px solid #cc8888")
                            .backgroundColor("#ffeeee");
            }
        }

        return html;
    }

}

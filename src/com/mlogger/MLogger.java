package com.mlogger;

import com.config.CService.ServiceMode;
import com.utils.console.TConsole;
import com.context.AppContext;
import com.mlogger.handlers.*;
import java.io.*;
import java.lang.Thread.State;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.SocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import com.mlogger.interfaces.ILogEventsHandler;
import com.utils.Utils;
import com.utils.Is;
import com.context.Environment;
import com.mlogger.status.StatusProcessor;
import com.mlogger.utils._Internal;
import com.utils.Is;
import com.utils.Str;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import java.util.*;

public class MLogger extends MLoggerMethods {

    private static MLogger instance;
    private String displayName = "mlogger";
    public final LogDefaults defaults = new LogDefaults(this);
    public final LogOptions options = new LogOptions(this);
    //public final static String version;

    public final static StatusProcessor processor = new StatusProcessor();

    public static MLogger instance() {

        if (instance == null) {
            Logger logger = Logger.getLogger("mlogger");
            instance = (logger != null && logger instanceof MLogger)
                    ? (MLogger) logger
                    : new MLogger("default");
        }
        return instance;
    }

    public boolean register() {
        return LogManager.getLogManager().addLogger(this);
    }

    private final BlockingQueue<LogRecord> queue = new ArrayBlockingQueue<LogRecord>(1024);

    private final Thread thread = new Thread(new Runnable() {

        @Override
        public void run() {

            thread.setName(getLoggerName());

            while (!thread.isInterrupted())
                try {

                    LogRecord record = queue.take();
                    if (record == null || record.getParameters() == null)
                        continue;

                    MLogRec rec = null;
                    for (Object o : record.getParameters())
                        if (o instanceof MLogRec)
                            rec = (MLogRec) o;

                    if (rec == null)
                        continue;

                    if (rec.log.options.minDelay > 0) {
                        long diff = System.currentTimeMillis() - rec.log.createdTS;

                        // odczekaj minimalny czas opoznienia
                        if (diff >= 0 && diff <= rec.log.options.minDelay)
                            Thread.sleep(diff);
                    }

                    while (!rec.handlers.isEmpty())
                        try {
                            Handler hnd = rec.handlers.poll();
                            if (hnd != null)
                                hnd.publish(record);
                        } catch (Throwable e) {
                            TConsole.printErr(e);
                        }

                } catch (InterruptedException e) {
                    return;
                } catch (Exception e) {
                    TConsole.printErr(e);
                }
        }

    });

    public String getLoggerName() {
        return "MLogger " + getLoggerDisplayName() + (AppContext.isInitialized()
                ? " " + AppContext.fraStatus.version + "." + AppContext.fraStatus.build
                : "");
    }

    @Override
    public void log(LogRecord record) {

        LogElement le = null;
        try {
            if (record == null)
                return;

            List<Handler> handlers = Arrays.asList(getHandlers());
            handlers.remove(MHandler.handler);
            Object[] params = record.getParameters();

            if (params != null && params.length > 0 && params[0] instanceof LogElement)
                le = (LogElement) params[0];

            if (le == null) {
                Log log = new Log(LogKind.DEBUG);
                le = log;
                log.logger.set("java logging / " + getLoggerName());

                Level level = record.getLevel();
                if (level == Level.SEVERE)
                    log.kind.set(LogKind.ERROR);
                if (level == Level.WARNING)
                    log.kind.set(LogKind.WARNING);

                if (Is.in(level, Level.INFO, Level.FINEST,
                        Level.FINER, Level.FINE, Level.CONFIG))
                    log.kind.set(LogKind.LOG);

                log.date.set(new TDate(record.getMillis()));
                log.value(record.getMessage());
                log.level.set(level.intValue());
                log.levelName.set(level.getName());
                String src = record.getSourceClassName();

                if (src != null)
                    for (String s : log.options.knowLoggers)
                        if (src.startsWith(s)) {
                            src = null;
                            break;
                        }

                if (src != null) {
                    log.clazz.set(record.getSourceClassName());
                    log.method.set(null);
                }

                log.attribute("res", record.getResourceBundleName());
                log.attribute("nbr", record.getSequenceNumber());

                int parIdx = 1;

                if (params != null)
                    for (Object o : params)
                        if (o != null && !(o instanceof Log))
                            log.data("Param " + parIdx++, Utils.toString(o));

                Throwable thrown = record.getThrown();

                if (thrown != null) {
                    log.value.set(null);
                    log.setErrorStackTrace(thrown)
                            .setExceptionDetails(thrown);
                    if (level != Level.SEVERE)
                        log.kind.set(LogKind.WARNING);
                }
            }

            if (thread.getState() == State.NEW)
                thread.start();

            queue.offer(new MLogRec(le).process(record));

        } catch (Throwable e) {
            _Internal.onException(e, null, le);
        }
    }

    public void addLog(LogElement log) {
        try {
            com.mlogger.console.Console.addLog(log);
            LogRecord record = new LogRecord(Level.OFF, null);
            record.setParameters(new Object[]{log});
            log(record);
        } catch (Exception e) {
            _Internal.onException(e, null, log);
        }
    }

    public void setLoggerDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getLoggerDisplayName() {
        return displayName;
    }

    public MLogger setSourceName(String sourceName) {
        defaults.source = sourceName;
        return this;
    }

    public String getSourceName() {
        return defaults.source;
    }

    public MLogger setMode(ServiceMode mode) {
        defaults.mode = mode;
        return this;
    }

    public MLogger addKey(String key) {
        defaults.keys.add(key);
        return this;
    }

    public MLogger(String displayName) {
        super();
        this.displayName = displayName;

        Logger parent = Logger.getLogger("mlogger");
        if (parent != null && parent instanceof MLogger)
            setParent(parent);

        if (instance == null)
            instance = this;

        try {
            defaults.loggerName = getLoggerName();

            Str path = new Str(URLDecoder.decode(LogOptions.class.getProtectionDomain()
                    .getCodeSource().getLocation().getPath(), Charset.defaultCharset().name()))
                    .removeSufix("build/classes/")
                    .removeSufix("target/classes/")
                    .removeSufix("bin/tmp/")
                    .removeSufix("bin/");

            defaults.source = new File(path.toString()).getName();
            defaults.user = System.getProperty("user.name");
            defaults.hostName = Environment.hostname;
            defaults.os = Environment.osName;
            defaults.instance = Utils.randomId(10);

            // ----------------------------------------------------------------
            LogManager manager = LogManager.getLogManager();

            String s = manager.getProperty("mlogger.name");
            if (s != null && !s.isEmpty())
                defaults.source = s.trim();

            s = manager.getProperty("mlogger.device");
            if (s != null && !s.isEmpty())
                defaults.device = s.trim();
            /*
            String res = manager.getProperty("mlogger.resources");
            if (res != null)
                for (String nn : res.split(","))
                    for (StatusResource rs : StatusResource.values())
                        if (rs.name().equalsIgnoreCase(nn.trim()))
                            rs.enabled = true;
             */
            String hosts = manager.getProperty("mlogger.host");

            if (hosts != null)
                for (String host : hosts.split(",")) {

                    if (host == null || host.trim().isEmpty())
                        continue;
                    int port = 514;

                    String[] addr = host.split(":");

                    if (addr.length == 0)
                        continue;

                    host = addr[0].trim();

                    try {
                        if (addr.length > 1)
                            port = Integer.parseInt(addr[1].trim());
                    } catch (Exception e) {
                    }

                    addHandlers(new UdpHandler(new InetSocketAddress(host, port)));
                }

            String name = ManagementFactory.getRuntimeMXBean().getName();

            if (name != null && name.contains("@"))
                defaults.processId = Long.parseLong(name.substring(0, name.indexOf("@")));

            //  MLogger.instance.config("");
        } catch (Throwable e) {
            TConsole.printErr(e);
        }

        // StatusMonitor.start();
        MHandler handler = MHandler.getInstance();

    }

    @Override
    public String toString() {
        return "MLogger" + (displayName != null && !displayName.isEmpty()
                ? " " + displayName : "");
    }

    public static void logInfo() {

        Log log = new Log(LogKind.INFO);
        try {
            log.value("MLogger info");

            /* for (LogHandler ch : MLogger.options.protocols)
             if (ch != null)
             log.attribute("Kanaly", null, ch);
             */
            log.attribute("Sciezka", LogDefaults.class.getProtectionDomain()
                    .getCodeSource().getLocation().getPath());

            RuntimeMXBean mxBean = ManagementFactory.getRuntimeMXBean();

            for (String s : mxBean.getInputArguments())
                log.attribute("Parametry maszyny wirtualnej", null, s);

            for (Entry<Object, Object> en : System.getProperties().entrySet())
                if (en.getKey() != null)
                    log.attribute("Wlasciwosci", en.getKey().toString(), en.getValue());

            log.send();

        } catch (Throwable e) {
            _Internal.onException(e, null, log);
        }
    }

    public ConsoleHandler addConsoleHandler() {
        ConsoleHandler handler = new ConsoleHandler();
        addHandler(handler);
        return handler;
    }

    public List<Handler> getHandlers(Class<? extends Handler> handler) throws SecurityException {
        List<Handler> list = new LinkedList<>();
        for (Handler h : getHandlers())
            if (h != null && h.getClass().isAssignableFrom(handler))
                list.add(h);
        return list;
    }

    @Override
    public void addHandler(Handler handler) throws SecurityException {

        boolean containSameClass = false;
        for (Handler h : getHandlers()) {
            containSameClass = handler.getClass() == h.getClass();
            if (Objects.equals(h, handler))
                return;
        }

        if (handler instanceof ConsoleHandler && containSameClass)
            return;

        super.addHandler(handler);
    }

    public UdpHandler addUdpHandler(SocketAddress address) {
        UdpHandler hnd = new UdpHandler(address);
        addHandler(hnd);
        return hnd;
    }

    public UdpHandler addUdpHandler(String host, int port) {
        return addUdpHandler(new InetSocketAddress(host, port > 0 ? port : 514));
    }

    public TcpHandler addTcpHandler(String host, int port) {
        return addTcpHandler(new InetSocketAddress(host, port > 0 ? port : 514));
    }

    public TcpHandler addTcpHandler(SocketAddress address) {
        TcpHandler hnd = new TcpHandler(address);
        addHandler(hnd);
        return hnd;
    }

    public HttpHandler addHttpHandler(URL url) throws MalformedURLException {
        HttpHandler hnd = new HttpHandler(url);
        addHandler(hnd);
        return hnd;
    }

    public TextFileHandler addFileHandler(File path) throws FileNotFoundException {
        TextFileHandler hnd = new TextFileHandler(path);
        addHandler(hnd);
        return hnd;
    }

    public HttpHandler addHttpHandler(URI uri) throws MalformedURLException {
        return addHttpHandler(new URL(uri.getScheme(), uri.getHost(), uri.getPort(), uri.getPath()));
    }

    public TelnetHandler addTelnetHandler(URI uri) throws IOException {
        TelnetHandler hnd = new TelnetHandler(uri);
        addHandler(hnd);
        return hnd;
    }

    public HttpHandler addHttpHandler(String url) throws MalformedURLException {
        return addHttpHandler(new URL(url));
    }

    public void addHandlers(Handler... handlers) {
        if (handlers == null || handlers.length == 0)
            return;
        for (Handler hnd : handlers)
            addHandler(hnd);
    }

    public MLogger clearHandlers() {
        for (Handler h : getHandlers()) {
            if (h instanceof LogHandler)
                ((LogHandler) h).stop();
            removeHandler(h);
        }
        return this;
    }

    public static LogHandler getHandlerFromUrl(String url) throws MalformedURLException, URISyntaxException, IOException {
        if (url == null || url.trim().isEmpty())
            return null;

        url = url.trim();

        URI uri = new URI(url);
        String protocol = uri.getScheme();
        if (protocol == null)
            return null;

        protocol = protocol.toLowerCase().trim();

        if (protocol.equals("udp"))
            return new UdpHandler(new InetSocketAddress(uri.getHost(), uri.getPort() > 0
                    ? uri.getPort() : 514));

        if (protocol.equals("tcp"))
            return new TcpHandler(new InetSocketAddress(uri.getHost(), uri.getPort() > 0
                    ? uri.getPort() : 514));

        if (protocol.equals("http") || protocol.equals("https"))
            return new HttpHandler(new URL(url));

        if (protocol.equals("file"))
            return new TextFileHandler(new File(url.substring(url.indexOf("://") + 3)));

        if (protocol.equals("console"))
            return new ConsoleHandler();

        if (protocol.equals("telnet"))
            return new TelnetHandler(uri);

        return null;
    }

    public Handler addHandler(String url) {
        if (url == null || url.trim().isEmpty())
            return null;
        try {

            LogHandler handler = getHandlerFromUrl(url);
            if (handler != null)
                addHandlers(handler);

        } catch (Exception e) {
            _Internal.onException(e, null, null);
        }
        return null;
    }

    public List<Handler> addHandlers(String... urls) {
        List<Handler> list = new LinkedList<Handler>();
        if (urls == null || urls.length == 0)
            return list;
        for (String sUrl : urls)
            list.add(addHandler(sUrl));
        return list;
    }

    public void addEventsHandler(ILogEventsHandler handler) {
        synchronized (options.events) {
            options.events.add(handler);
        }
    }

    public void shutdown() {
        clearHandlers();
        thread.interrupt();
    }

    static void cleanupThreadsList() {
        try {
            ThreadGroup group = Thread.currentThread().getThreadGroup();
            while (group.getParent() != null)
                group = group.getParent();
            Thread[] threads = new Thread[group.activeCount()];
            group.enumerate(threads, true);

            synchronized (LogDefaults.threadData) {
                List<Thread> lst = new LinkedList<Thread>();

                for (Thread thd : LogDefaults.threadData.keySet()) {
                    boolean found = false;
                    for (Thread th : threads)
                        if (th == thd) {
                            found = true;
                            break;
                        }
                    if (!found)
                        lst.add(thd);
                }

                for (Thread th : lst)
                    LogDefaults.threadData.remove(th);
            }

            synchronized (LogOptions.threadData) {
                List<Thread> lst = new LinkedList<Thread>();

                for (Thread thd : LogOptions.threadData.keySet()) {
                    boolean found = false;
                    for (Thread th : threads)
                        if (th == thd) {
                            found = true;
                            break;
                        }
                    if (!found)
                        lst.add(thd);
                }

                for (Thread th : lst)
                    LogOptions.threadData.remove(th);
            }

        } catch (Exception e) {
            _Internal.onException(e, null, null);
        }
    }

    public class MLogRec {

        public final LogElement log;
        public boolean priority;
        public final LinkedList<Handler> handlers = new LinkedList<>();

        private void addHandler(LogElement log, Handler handler) {

            for (ILogEventsHandler evn : log.options.events)
                if (log instanceof Log && !evn.onBeforeSend(handler, (Log) log))
                    return;

            handlers.add(handler);
        }

        public LogRecord process(LogRecord record) {

            ArrayList<Object> list = new ArrayList<Object>();
            if (record.getParameters() != null)
                list.addAll(Arrays.asList(record.getParameters()));
            list.add(this);
            record.setParameters(list.toArray());

            if (!log.handlers.isEmpty())
                for (Handler h : log.handlers)
                    addHandler(log, h);
            else
                for (Handler h : getHandlers())
                    addHandler(log, h);

            return record;
        }

        private MLogRec(LogElement log) {
            this.log = log;
        }

    }

}

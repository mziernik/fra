package com.mlogger;

import com.config.CService.ServiceMode;
import com.utils.text.StrWriter;
import com.exceptions.EError;
import com.exceptions.EError.SourceCode;
import com.exceptions.EError.SourceCode.SourceCodeLine;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.UUID;
import com.mlogger.handlers.LogHandler;
import com.mlogger.interfaces.ILogEventsHandler;
import com.mlogger.interfaces.LogException;
import com.mlogger.utils.JsonBuilder;
import com.utils.Utils;
import com.utils.Is;
import com.context.index.Index;
import com.intf.runnable.Runnable1;
import com.mlogger.LogEntry.LogEntries;
import com.mlogger.LogEntry.LogEntryMap;
import com.mlogger.utils._Internal;
import com.utils.collections.*;
import com.utils.date.TDate;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

import static com.mlogger.LogAttr.*;
import com.thread.ThreadObject;
import com.thread.ThreadDump;
import com.utils.Is;

/**
 * Miłosz Ziernik 2013/04/03
 */
public class Log extends LogElement {

    private final static AtomicLong globalCounter = new AtomicLong();

    public final List<LogEntry<?>> entries = new LinkedList<>();
    public final LogEntry<String> logger = new LogEntry<>(this, LOGGER);
    public final LogEntry<String> clazz = new LogEntry<>(this, CLAZZ);
    public final LogEntry<String> method = new LogEntry<>(this, METHOD);
    public final LogEntry<Integer> level = new LogEntry<>(this, LEVEL);
    public final LogEntry<String> levelName = new LogEntry<>(this, LEVEL_NAME);
    public final LogEntry<UUID> uid = new LogEntry<>(this, UID);
    public final LogEntry<LogKind> kind = new LogEntry<>(this, KIND);
    public final LogEntry<TDate> date = new LogEntry<>(this, DATE);
    public final LogEntries<String> keys = new LogEntries<>(this, KEYS);
    public final LogEntry<ServiceMode> mode = new LogEntry<>(this, MODE);
    public final LogEntry<Long> counter = new LogEntry<>(this, COUNTER);
    public final LogEntry<String> source = new LogEntry<>(this, SOURCE);
    public final LogEntries<String> address = new LogEntries<>(this, ADDRESS);
    public final LogEntry<String> device = new LogEntry<>(this, DEVICE);
    public final LogEntry<String> os = new LogEntry<>(this, OS);
    public final LogEntry<String> hostName = new LogEntry<>(this, HOST_NAME);
    public final LogEntry<String> userAgent = new LogEntry<>(this, USER_AGENT);
    public final LogEntry<String> user = new LogEntry<>(this, USER_NAME);
    public final LogEntries<String> tag = new LogEntries<>(this, TAG);
    public final LogEntry<DataObj> value = new LogEntry<>(this, VALUE);
    public final LogEntry<String> comment = new LogEntry<>(this, COMMENT);
    public final LogEntry<String> instance = new LogEntry<>(this, INSTANCE);
    public final LogEntry<String> session = new LogEntry<>(this, SESSION);
    public final LogEntry<String> request = new LogEntry<>(this, REQUEST);
    public final LogEntry<String> version = new LogEntry<>(this, VERSION);
    public final LogEntry<Long> processId = new LogEntry<>(this, PROCESS);
    public final LogEntry<Long> threadId = new LogEntry<>(this, THREAD);
    public final LogEntry<String> threadName = new LogEntry<>(this, THREAD_NAME);
    public final LogEntry<Integer> threadPriority = new LogEntry<>(this, THREAD_PRIORITY);
    public final LogEntry<String> color = new LogEntry<>(this, COLOR);
    public final LogEntry<String> background = new LogEntry<>(this, BACKGROUND);
    public final LogEntries<String> url = new LogEntries<>(this, URL);
    public final LogEntries<LinkedList<String>> callStack = new LogEntries<>(this, CALL_STACK);
    public final LogEntries<LinkedList<String>> errorStack = new LogEntries<>(this, ERROR_STACK);
    public final LogEntryMap<DataPairs> attributes = new LogEntryMap<>(this, ATTRIBUTE);
    public final LogEntries<DataObj> data = new LogEntries<>(this, DATA);
    public final LogEntry<SourceCode> sourceCode = new LogEntry<>(this, SOURCE_CODE);
    public final LogEntries<String> flags = new LogEntries<>(this, FLAGS);
    public final LogEntry<String> fieldKey = new LogEntry<>(this, FIELD_KEY);
    public final LogEntry<Double> progress = new LogEntry<>(this, PROGRESS);
    public final LogEntry<String> group = new LogEntry<>(this, GROUP);
    private Throwable exception;

    //------------------------
    public Long id;

    public final HashMap<String, Object> extra = new HashMap<>(); // dane użytkownika

    public static Log log(Object value) {
        return MLogger.instance().log(value);
    }

    public static Log log(String tag, Object value) {
        return MLogger.instance().log(tag, value);
    }

    public static Log log(String tag, Object value, Object details) {
        return MLogger.instance().log(tag, value, details);
    }

    public static Log trace(Object value) {
        return MLogger.instance().trace(value);
    }

    public static Log trace(String tag, Object value) {
        return MLogger.instance().trace(tag, value);
    }

    public static Log trace(String tag, Object value, Object details) {
        return MLogger.instance().trace(tag, value, details);
    }

    public static Log info(Object value) {
        return MLogger.instance().info(value);
    }

    public static Log info(String tag, Object value) {
        return MLogger.instance().info(tag, value);
    }

    public static Log info(String tag, Object value, Object details) {
        return MLogger.instance().info(tag, value, details);
    }

    public static Log debug(Object value) {
        return MLogger.instance().debug(value);
    }

    public static Log debug(String tag, Object value) {
        return MLogger.instance().debug(tag, value);
    }

    public static Log debug(String tag, Object value, Object details) {
        return MLogger.instance().debug(tag, value, details);
    }

    public static Log warning(Object value) {
        return MLogger.instance().warning(value);
    }

    public static Log warning(String tag, Object value) {
        return MLogger.instance().warning(tag, value);
    }

    public static Log warning(String tag, Object value, Object details) {
        return MLogger.instance().warning(tag, value, details);
    }

    public static Log warning(Throwable ex) {
        return MLogger.instance().warning(ex);
    }

    public static Log warning(String tag, Throwable ex) {
        return MLogger.instance().warning(tag, ex);
    }

    public static Log warning(String tag, Throwable ex, Object details) {
        return MLogger.instance().warning(tag, ex, details);
    }

    public static Log error(Object value) {
        return MLogger.instance().error(value);
    }

    public static Log error(String tag, Object value) {
        return MLogger.instance().error(tag, value);
    }

    public static Log error(String tag, Object value, Object details) {
        return MLogger.instance().error(tag, value, details);
    }

    public static Log error(Throwable ex) {
        return MLogger.instance().error(ex);
    }

    /*
    public static Log exception(Throwable ex) {
        return MLogger.instance().exception(ex);
    }
     */
    public static Log error(String tag, Throwable ex) {
        return MLogger.instance().error(tag, ex);
    }

    public static Log error(String tag, Throwable ex, Object details) {
        return MLogger.instance().error(tag, ex, details);
    }

    public static Log event(Object value) {
        return MLogger.instance().event(value);
    }

    public static Log event(String tag, Object value) {
        return MLogger.instance().event(tag, value);
    }

    public static Log event(String tag, Object value, Object details) {
        return MLogger.instance().event(tag, value, details);
    }

    public Log setException(Throwable exception) {
        this.exception = exception;
        return this;
    }

    public Throwable getException() {
        return exception;
    }

    /**
     * Loguj biezaca procedure
     */
    public static Log currentMethod() {
        return MLogger.instance().currentMethod();
    }

    private synchronized static long getCounter() {
        return globalCounter.incrementAndGet();
    }

    public Log(LogKind kind, LogDefaults defs, MLogger logger) {
        this(logger, defs);
        this.kind.set(kind);
    }

    protected Log() {
        super(MLogger.instance());
    }

    public Log(LogKind kind, MLogger logger) {
        this(logger, new LogDefaults(null));
        this.kind.set(kind);
    }

    public Log(LogKind kind) {
        this(kind, MLogger.instance());
    }

    private Log(MLogger logger, LogDefaults defs) {
        super(logger);
        this.uid.set(UUID.randomUUID());
        this.counter.set(getCounter());
        this.kind.set(LogKind.DEBUG);

        this.date.set(new TDate());
        this.logger.set(getLogger().getLoggerName());
        Thread thread = Thread.currentThread();
        this.threadId.set(thread.getId());
        this.threadName.set(thread.getName());
        this.threadPriority.set(thread.getPriority());

        if (defs == null)
            return;
        defs.loadThreadDefaults();
        this.mode.set(defs.mode);
        this.keys.addAll(defs.keys);
        this.tag.addAll(defs.tags);
        this.source.set(defs.source);
        this.user.set(defs.user);
        this.device.set(defs.device);
        this.hostName.set(defs.hostName);
        this.os.set(defs.os);
        this.session.set(defs.session);
        this.instance.set(defs.instance);
        this.processId.set(defs.processId);
        this.comment.set(defs.comment);
        this.address.addAll(defs.addresses);
        this.request.set(defs.request);
        this.color.set(defs.color);
        this.background.set(defs.background);
        this.url.addAll(defs.urls);
        this.version.set(defs.version);
        this.logger.set(defs.loggerName);
        this.userAgent.set(defs.ua);
        this.os.set(defs.os);
        this.hostName.set(defs.hostName);
        this.level.set(defs.level);
        this.handlers.addAll(defs.handlers);

        for (DataObj dd : defs.data)
            this.data.add(new DataObj(dd.name, dd.value, dd.type));

        this.attributes.addAll(defs.attributes);
        this.flags.addAll(defs.flags);
        this.extra.putAll(defs.extra);

        for (ILogEventsHandler handler : Collections.synchronizedCollection(options.events))
            try {
                handler.onAfterCreate(this);
            } catch (Exception e) {
                _Internal.onException(e, null, this);
            }

        ThreadDump dump = new ThreadDump();

        String pck = getClass().getPackage().getName();

        boolean rootFound = false;

        for (StackTraceElement el : dump.stackTrace) {
            String name = el.getClassName();

            if (name == null)
                continue;

            if (!rootFound && name.startsWith(pck))
                rootFound = true;

            for (String s : options.knowLoggers)
                if (name.startsWith(s)) {
                    name = null;
                    break;
                }

            if (name == null)
                continue;

            if (!rootFound)
                continue;

            this.clazz.set(name);
            String mth = el.getMethodName();

            String file = el.getFileName();
            int line = el.getLineNumber();

            if ((file != null && !file.isEmpty()) || line > 0) {
                mth += " (";
                if (file != null && !file.isEmpty())
                    mth += file;
                if (line > 0)
                    mth += ":" + line;
                mth += ")";
            }
            this.method.set(mth);
            break;
        }

        addStackTrace(dump);

        Is.notNullV(ThreadObject.parentThread.get(), (pt) -> {
            addStackTrace(pt.parent);
        });
    }

    private void addStackTrace(ThreadDump dump) {
        if (dump == null)
            return;
        LinkedList<String> list = new LinkedList<>();
        formatStackTrace(dump.stackTrace, list, false);
        if (!list.isEmpty()) {
            list.add(0, "[" + dump.date.toString("HH:mm:ss.SSS") + ", " + dump.id
                    + ", \"" + dump.name + "\"]");
            callStack.add(list);
        }
        addStackTrace(dump.parent);
    }

    public Log className(String className) {
        this.clazz.set(className);
        return this;
    }

    public Log level(int level) {
        this.level.set(level);
        return this;
    }

    public Log levelName(String levelName) {
        return value(levelName, DataType_old.TEXT);
    }

    public Log source(String source) {
        this.source.value = source;
        return this;
    }

    public Log tag(String... tags) {

        List<String> lst = new LinkedList<>();

        if (tags != null)
            for (String s : tags)
                if (!Is.empty(s))
                    lst.addAll(Arrays.asList(s.split("\\|")));

        if (tags != null)
            this.tag.addAll(lst);
        return this;
    }

    public Log value(Object value) {
        return value(value, DataType_old.TEXT);
    }

    public Log value(Object value, DataType_old type) {
        this.value.set(new DataObj("", Utils.toString(value), type));
        return this;
    }

    public Log details(Object details) {
        return details(details, DataType_old.TEXT);
    }

    public Log details(Object details, DataType_old type) {
        this.data.add(new DataObj("Szczegóły", Utils.toString(details), type));
        return this;
    }

    public Log comment(Object comment) {
        this.comment.set(Utils.toString(comment));
        return this;
    }

    public Log address(String... addresses) {
        if (addresses != null)
            this.address.addAll(Arrays.asList(addresses));
        return this;
    }

    public Log device(String device) {
        return this.device.set(device);
    }

    public Log userAgent(String device) {
        return this.userAgent.set(device);
    }

    public Log hostName(String device) {
        return this.hostName.set(device);
    }

    public Log user(String user) {
        return this.user.set(user);
    }

    public Log color(String color) {
        return this.color.set(color);
    }

    public Log background(String background) {
        return this.background.set(background);
    }

    public Log url(String... urls) {
        if (urls != null)
            this.url.addAll(Arrays.asList(urls));
        return this;
    }

    public Log attribute(Object value) {
        return attribute(null, null, value);
    }

    public Log attribute(String name, Object value) {
        return attribute(null, name, value);
    }

    public Log data(String name, Object value) {
        if (value == null)
            return this;
        return data.add(new DataObj(name, Utils.toString(value), DataType_old.TEXT));
    }

    public Log data(String name, Object value, DataType_old type) {
        data.add(new DataObj(name, Utils.toString(value), type));
        return this;
    }

    public Log attribute(String group, String name, Object value) {
        DataPairs lst = attributes.get(group);
        if (lst == null) {
            lst = new DataPairs();
            attributes.add(group, lst);
        }
        lst.add(new DataPair(name, Utils.toString(value)));
        return this;
    }

//    public final static String dateFormat = "yyyy-MM-dd HH:mm:ss.SSS";
    public static LogDefaults currentThreadDefaults() {
        return LogDefaults.setCurrentThreadDefaults();
    }

    public Log addFields(Object object) {

        if (object == null)
            return this;

        try {
            for (Field f : object.getClass().getDeclaredFields()) {
                f.setAccessible(true);
                attribute(f.getName(), Utils.toString(f.get(object)));
            }
        } catch (Throwable e) {
            _Internal.onException(e, null, this);
        }

        return this;
    }

    public Log setErrorStackTrace(final Throwable ex) {
        if (ex == null)
            return this;
        /*
         Throwable e = ex;
         String cause = "";

         while (e != null) {
         String s = e.getLocalizedMessage();
         if (s == null)
         s = e.getMessage();
         cause += e.getClass().getSimpleName() + (s != null
         ? ": " + e.getLocalizedMessage() : "") + "\n";
         e = e.getCause();
         }

         cause += "\n";

         e = ex;

         EError err = new EError(ex);

         tags.addAll(err.shortClasses.getList());

         if (value == null
         || value.value == null
         || value.value.toString() == null
         || value.value.toString().isEmpty())
         value(err.toString(false));
         data("Błąd", cause); */

        Throwable e = ex;
        while (e != null) {
            LinkedList<String> lst = new LinkedList<>();
            lst.add(e.toString());
            formatStackTrace(e.getStackTrace(), lst, true);
            this.errorStack.add(lst);
            e = e.getCause();
        }

        return this;
    }

    public Log send() {

        Is.notNullV(ThreadObject.logsHandler.get(), r -> r.run(this));

        getLogger().addLog(this);

        if (Is.in(kind.value, LogKind.WARNING, LogKind.ERROR, LogKind.EXCEPTION))
            for (ILogEventsHandler handler : Collections.synchronizedCollection(options.events))
                try {
                    handler.onError(this, exception);
                } catch (Exception e) {
                    _Internal.onException(e, null, this);
                }

        return this;
    }

    @Override
    public void toJson(JsonBuilder jw, LogHandler handler) {
        jw.obj('{');

        String lname = logger.value();
        if (handler != null) {
            if (lname == null)
                lname = "";
            lname += " via " + handler.toString();
        }

        jw.pair(counter);
        if (mode.value() != null)
            jw.pair(mode, mode.value().key);
        if (!keys.isEmpty())
            jw.pair(keys);
        jw.pair(logger, lname);

        String sDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(date.value(new TDate()));

        //   sDate += "000";
        jw.pair(date, sDate)
                .pair(uid)
                .pair(source);

        jw.pair(kind, kind.isEmpty() ? null : kind.value().name().toLowerCase());
        jw.pair(level);
        jw.pair(levelName);
        jw.pair(clazz);
        jw.pair(method);
        jw.array(address);
        jw.array(tag);
        jw.array(url);

        if (value.value != null)
            jw.name(value.key).dataObj(value.value(), value);

        jw.pair(device);
        jw.pair(os);
        jw.pair(hostName);
        jw.pair(userAgent);
        jw.pair(user);
        jw.pair(comment);
        jw.pair(version);
        jw.pair(request);
        jw.pair(session);
        jw.pair(processId);
        jw.pair(threadId);
        jw.pair(threadName);
        jw.pair(threadPriority);
        jw.pair(instance);
        jw.pair(color);
        jw.pair(background);

        // kompatybilnosc wsteczna
        LinkedList<String> stack = new LinkedList<>();
        if (errorStack.value != null)
            for (List<String> lst : errorStack.value()) {
                if (!stack.isEmpty())
                    stack.add("--");
                stack.addAll(lst);
            }

        jw.array(errorStack, stack);

        stack = new LinkedList<>();
        if (callStack.value != null)
            for (List<String> lst : callStack.value()) {
                if (!stack.isEmpty())
                    stack.add("--");
                stack.addAll(lst);
            }

        jw.array(callStack, stack);

        if (sourceCode.value != null) {
            SourceCode src = sourceCode.value();
            jw.name(sourceCode.key);
            jw.obj('[');

            jw.value(src.fileName)
                    .value(src.number)
                    .value(src.method);

            for (SourceCodeLine scl : src.lines)
                jw.obj('[')
                        .value(scl.number)
                        .value(scl.value)
                        .obj(']');

            jw.obj(']');
        }

        if (!data.isEmpty()) {
            jw.name(data.key);
            jw.obj('[');
            for (DataObj dd : data)
                jw.dataObj(dd, data);
            jw.obj(']');
        }

        if (!attributes.isEmpty()) {
            jw.name(attributes.key);
            jw.obj('{');

            for (Entry<String, DataPairs> entry : attributes) {
                jw.nameEsc(entry.getKey());
                jw.obj('[');
                for (DataPair pair : entry.getValue())
                    jw.dataPair(pair, attributes);
                jw.obj(']');
            }
            jw.obj('}');
        }

        if (!flags.isEmpty()) {
            jw.name("flg").obj('[');
            for (String s : flags)
                jw.value(s, flags.maxValueLength);

            jw.obj(']');
        }

        jw.obj('}');
    }

    @Override
    public String toString(LogHandler handler) {
        JsonBuilder jw = new JsonBuilder();

        jw.obj('{')
                .pair("ver", currentVersion, 3)
                .name("log");

        toJson(jw, handler);

        jw.obj('}');
        return jw.toString();
    }

    private void formatStackTrace(StackTraceElement[] stackTrace, Collection<String> list, boolean isErrorStack) {
        try {

            String pck = getClass().getPackage().getName();

            boolean rootFound = true;

            int cnt = 0;
            for (StackTraceElement ste : stackTrace) {
                StrWriter sb = new StrWriter();

                String cls = ste.getClassName();
                if (cls == null)
                    continue;

                if (!rootFound && cls.startsWith(pck))
                    rootFound = true;

                if (!rootFound)
                    continue;

                if (!isErrorStack)
                    for (String s : options.knowLoggers)
                        if (cls.startsWith(s)) {
                            cls = null;
                            break;
                        }

                if (cls == null)
                    continue;

                String fname = Utils.coalesce(ste.getClassName(), "").replace(".", "/");

                if (fname.contains("/"))
                    fname = fname.substring(0, fname.lastIndexOf("/"));

                if (ste.getFileName() != null)
                    fname += "/" + ste.getFileName().replace(".java", ".class");

                if (Index.svrIdx.files.contains(fname))
                    sb.append("*");
                else if (Index.fraIdx.files.contains(fname))
                    sb.append("+");

                sb.append(ste.getClassName()).append(".").append(ste.getMethodName());

                if (ste.getFileName() != null && ste.getLineNumber() > 0) {
                    sb.append(" (").append(ste.getFileName()).append(":");
                    sb.append(ste.getLineNumber()).append(")");
                }
                list.add(sb.toString());
                ++cnt;
                if (cnt > 50)
                    break;
            }

        } catch (Exception e) {
            _Internal.onException(e, null, this);
        }
    }

    public void sendPriority() throws LogException {
        options.priority = true;
        send();
    }

    public Log setSourceCode(Throwable ex) {
        sourceCode.set(new EError(ex).getSourceCodeLine());
        return this;
    }

    public Log setExceptionDetails(Throwable exception) {
        Throwable e = exception;

        LinkedHashSet<String> vals = new LinkedHashSet<>();

        int level = 0;

        EError err = new EError(exception);

        String logValue = Utils.coalesce(value != null ? Utils.toString(value.value) : null, "");

        for (String s : err.shortClasses)
            if (!tag.contains(s))
                tag.add(s);

        if (Is.empty(logValue)) {
            logValue = err.toString(false);
            value(logValue);
        }

        while (e != null && level < 10)
            try {
                for (Field f : e.getClass().getDeclaredFields())
                    try {

                        int mods = f.getModifiers();
                        if (Modifier.isStatic(mods))
                            continue;

                        f.setAccessible(true);
                        Object obj = f.get(e);

                        String val = obj != null ? obj.toString() : null;
                        if (val == null || val.trim().isEmpty())
                            continue;

                        // wyeliminuj powtórzenia danych
                        if (logValue.contains(val))
                            continue;

                        val = Utils.cutLongName(val, data.maxValueLength, false);

                        String key = e.getClass().getSimpleName() + "." + f.getName();
                        if (val.contains(key + val))
                            continue;

                        for (String s : vals)
                            if (s.contains(val)) {
                                val = null;
                                break;
                            }

                        if (val == null)
                            continue;

                        data(key, obj);
                        vals.add(key + val);
                    } catch (Throwable e1) {
                    }

                e = e.getCause();
                ++level;
            } catch (Throwable ex) {
            }

        if (!err.details.isEmpty()) {

            Strings defs = new Strings();

            for (Entry<String, String> en : err.details.entrySet()) {
                String key = en.getKey();
                String val = en.getValue();

                if (val == null || val.isEmpty())
                    continue;

                if (key == null || !key.trim().isEmpty())
                    data(key, en.getValue());
                else
                    defs.add(val);
            }

            //          data("Szczegóły błędu", defs.toString("\n\n"));
        }
        return this;
    }

    public LogEntry<?> getEntry(final LogAttr attr) {
        return entries.stream().filter((LogEntry<?> le) -> {
            return le.attr == attr;
        }).findFirst().orElse(null);
    }

}

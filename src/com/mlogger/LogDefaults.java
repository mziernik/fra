package com.mlogger;

import com.config.CService.ServiceMode;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Handler;
import com.mlogger.LogElement.DataObj;
import com.utils.Utils;
import com.utils.Is;
import com.mlogger.LogElement.DataPairs;
import java.util.*;

public class LogDefaults {

    public ServiceMode mode = ServiceMode.DEV;
    public final LinkedHashSet<String> keys = new LinkedHashSet<>();
    public final LinkedHashSet<String> tags = new LinkedHashSet<>();
    public String source;
    public String user;
    public String device;
    public String hostName;
    public String ua;
    public String session;
    public String instance;
    public Long processId;
    public String comment;
    public final Set<String> addresses = new LinkedHashSet<>();
    public String request;
    public String color;
    public String background;
    public final Set<String> urls = new LinkedHashSet<>();
    public String version;
    public Integer expireConsole;
    public String loggerName;
    public Integer level;
    public final List<Handler> handlers = new LinkedList<>();

    public final Set<DataObj> data = new LinkedHashSet<>();
    public final Map<String, DataPairs> attributes = new LinkedHashMap<>();
    public final HashMap<String, String> properties = new HashMap<>();
    public final HashMap<String, Object> extra = new HashMap<>(); // dane użytkownika
    public final Set<String> flags = new HashSet<>();

    final static Map<Thread, LogDefaults> threadData = new HashMap<>();

    public final MLogger logger;
    public String os;

    public LogDefaults(MLogger logger) {
        this.logger = logger != null ? logger : MLogger.instance();
        load(this.logger.defaults);
    }

    public Log log(LogKind kind) {
        return new Log(kind, logger);
    }

    public Log debug() {
        return new Log(LogKind.DEBUG, logger);
    }

    public Log event() {
        return new Log(LogKind.EVENT, logger);
    }

    public Log request() {
        return new Log(LogKind.REQUEST, logger);
    }

    public Log warning() {
        return new Log(LogKind.WARNING, logger);
    }

    public Log warning(Throwable e) {
        return new Log(LogKind.WARNING, logger)
                .setException(e)
                .setErrorStackTrace(e);
    }

    public Log error() {
        return new Log(LogKind.ERROR, logger);
    }

    public Log error(Throwable e) {
        return new Log(LogKind.ERROR, logger)
                .setErrorStackTrace(e)
                .setException(e)
                .setExceptionDetails(e);
    }

    public Log info() {
        return new Log(LogKind.INFO, logger);
    }

    void load(LogDefaults parent) {
        if (parent == null)
            return;

        this.mode = parent.mode;
        this.keys.addAll(parent.keys);
        this.tags.addAll(parent.tags);
        this.source = parent.source;
        this.user = parent.user;
        this.device = parent.device;
        this.ua = parent.ua;
        this.hostName = parent.hostName;
        this.os = parent.os;
        this.session = parent.session;
        this.instance = parent.instance;
        this.processId = parent.processId;
        this.comment = parent.comment;
        this.addresses.addAll(parent.addresses);
        this.request = parent.request;
        this.color = parent.color;
        this.background = parent.background;
        this.urls.addAll(parent.urls);
        this.version = parent.version;
        this.expireConsole = parent.expireConsole;
        this.loggerName = parent.loggerName;
        this.level = parent.level;
        this.handlers.addAll(parent.handlers);

        for (DataObj dd : parent.data)
            this.data.add(new DataObj(dd.name, dd.value, dd.type));

        this.attributes.putAll(parent.attributes);
        this.properties.putAll(parent.properties);
        this.flags.addAll(parent.flags);
        this.extra.putAll(parent.extra);
    }

    public void loadThreadDefaults() {
        MLogger.cleanupThreadsList();
        load(threadData.get(Thread.currentThread()));
    }

    public static LogDefaults setCurrentThreadDefaults() {
        LogDefaults defs = new LogDefaults(null);
        synchronized (LogDefaults.threadData) {
            LogDefaults.threadData.put(Thread.currentThread(), defs);
        }
        return defs;
    }

    public LogDefaults tag(String... tags) {
        if (tags != null)
            this.tags.addAll(Arrays.asList(tags));
        return this;
    }

    public LogDefaults key(String... keys) {
        if (keys != null)
            this.keys.addAll(Arrays.asList(keys));
        return this;
    }

    public LogDefaults mode(ServiceMode mode) {
        this.mode = mode;
        return this;
    }

    public LogDefaults comment(Object comment) {
        this.comment = Utils.toString(comment);
        return this;
    }

    public LogDefaults address(String... addresses) {
        if (addresses != null)
            this.addresses.addAll(Arrays.asList(addresses));
        return this;
    }

    public LogDefaults device(String device) {
        this.device = device;
        return this;
    }

    public LogDefaults user(String user) {
        this.user = user;
        return this;
    }

    public LogDefaults color(String color) {
        this.color = color;
        return this;
    }

    public LogDefaults background(String background) {
        this.background = background;
        return this;
    }

    public LogDefaults url(String... urls) {
        if (urls != null)
            this.urls.addAll(Arrays.asList(urls));
        return this;
    }

    public LogDefaults property(String name, Object value) {
        this.properties.put(name, Utils.toString(value));
        return this;
    }

    public LogDefaults attribute(Object value) {
        attribute(null, null, value);
        return this;
    }

    public LogDefaults attribute(String name, Object value) {
        attribute(null, name, value);
        return this;
    }

    public LogDefaults data(String name, Object value) {
        data.add(new DataObj(name, Utils.toString(value), DataType.TEXT));
        return this;
    }

    public LogDefaults data(String name, Object value, DataType type) {
        data.add(new DataObj(name, Utils.toString(value), type));
        return this;
    }

    public LogDefaults attribute(String group, String name, Object value) {
        DataPairs pairs = attributes.get(group);
        if (pairs == null) {
            pairs = new DataPairs();
            attributes.put(group, pairs);
        }
        pairs.add(new LogElement.DataPair(name, value));
        return this;
    }

    public LogDefaults expire(Integer console) {
        this.expireConsole = console;
        return this;
    }

}

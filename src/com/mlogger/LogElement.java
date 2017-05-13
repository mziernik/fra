package com.mlogger;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.logging.Handler;
import com.mlogger.handlers.LogHandler;
import com.mlogger.utils.JsonBuilder;

public abstract class LogElement {

    public final LogOptions options;
    final long createdTS = System.currentTimeMillis();

    protected final static byte currentVersion = 3;
    public final List<Handler> handlers = new LinkedList<>();
    private MLogger logger;

    public MLogger getLogger() {
        return logger != null ? logger : MLogger.instance();
    }

    public MLogger setLogger(MLogger logger) {
        this.logger = logger != null ? logger : MLogger.instance();
        return this.logger;
    }

    public LogElement(MLogger logger) {
        this.logger = logger;
        this.options = new LogOptions(logger);
        ;
    }

    public static class DataObjs extends LinkedHashSet<DataObj> {

    }

    protected abstract void toJson(JsonBuilder json, LogHandler handler);

    public abstract String toString(LogHandler handler);

    @Override
    public String toString() {
        return toString(null);
    }

    public static class DataObj extends DataPair {

        public DataType type;

        public DataObj(final String name, final Object value, final DataType type) {
            super(name, value);
            this.type = type;
        }

        @Override
        public String toString() {
            return "[" + type + "] " + super.toString();
        }

    }

    public static class DataPairs extends LinkedList<DataPair> {

    }

    public static class DataPair {

        public String name;
        public Object value;

        public DataPair(final String name, final Object value) {
            this.name = name;
            this.value = value;
        }

        public boolean isEmpty() {
            return value == null || value.toString().isEmpty();
        }

        @Override
        public String toString() {
            return name + ": " + value;
        }
    }
}

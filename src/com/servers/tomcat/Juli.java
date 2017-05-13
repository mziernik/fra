package com.servers.tomcat;

import com.mlogger.Log;
import com.mlogger.LogKind;
import com.mlogger.interfaces.ILogEventsHandler;
import com.mlogger.utils._Internal;
import java.util.Collections;

public class Juli {

    public final String name;
    private static TomcatLogLevel level = TomcatLogLevel.info;

    public static enum TomcatLogLevel {

        trace(LogKind.TRACE, 0),
        debug(LogKind.DEBUG, 1),
        info(LogKind.INFO, 3),
        warn(LogKind.WARNING, 5),
        error(LogKind.ERROR, 7),
        fatal(LogKind.EXCEPTION, 9);

        public final int weight;
        public final LogKind kind;

        private TomcatLogLevel(LogKind kind, int weight) {
            this.weight = weight;
            this.kind = kind;
        }

    }

    public static void setLevel(TomcatLogLevel level) {
        Juli.level = level;
    }

    public static TomcatLogLevel getLevel() {
        return level;
    }

    public Juli(String name) {
        this.name = name;
    }

    private void log(TomcatLogLevel level, Object message, Throwable ex) {
        if (Juli.level == null || Juli.level.weight > level.weight)
            return;

        String tag = null;
        if (name != null) {
            tag = name;
            if (tag.contains("."))
                tag = tag.substring(tag.lastIndexOf(".") + 1);
        }

        new Log(ex != null && level.kind != LogKind.EXCEPTION ? LogKind.ERROR : level.kind)
                .tag("Tomcat", tag)
                .levelName(level.name())
                .value(message)
                .setException(ex)
                .setErrorStackTrace(ex)
                .className(name)
                .send();

    }

    public final void trace(Object message, Throwable ex) {
        log(TomcatLogLevel.trace, message, ex);
    }

    public final void debug(Object message, Throwable ex) {
        log(TomcatLogLevel.debug, message, ex);
    }

    public final void info(Object message, Throwable ex) {
        log(TomcatLogLevel.info, message, ex);
    }

    public final void warn(Object message, Throwable ex) {
        log(TomcatLogLevel.warn, message, ex);
    }

    public final void error(Object message, Throwable ex) {
        log(TomcatLogLevel.error, message, ex);
    }

    public final void fatal(Object message, Throwable ex) {
        log(TomcatLogLevel.fatal, message, ex);
    }

}

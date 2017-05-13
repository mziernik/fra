package com.mlogger.utils;

import com.utils.console.TConsole;
import java.util.Collections;
import java.util.logging.Handler;
import com.mlogger.LogElement;
import com.mlogger.MLogger;
import com.mlogger.interfaces.ILogEventsHandler;
import com.mlogger.interfaces.LogException;

public abstract class _Internal {

    public static void onException(Throwable e, Handler handler, LogElement log) {
        try {

            MLogger logger = log != null ? log.getLogger() : MLogger.instance();

            for (ILogEventsHandler ih : Collections.synchronizedCollection(logger.options.events))
                if (ih != null)
                    ih.onException(new LogException(handler, log, e));

        } catch (Throwable ex) {
            TConsole.printErr(e);
        }
    }

    public static void addInfo(LogElement log, Handler handler, String tag, String info) {

        MLogger logger = log != null ? log.getLogger() : MLogger.instance();
        try {
            for (ILogEventsHandler ih
                    : Collections.synchronizedCollection(logger.options.events))
                ih.onInfo(handler, log, tag, info);
        } catch (Throwable ex) {
            onException(ex, handler, log);
        }
    }

}

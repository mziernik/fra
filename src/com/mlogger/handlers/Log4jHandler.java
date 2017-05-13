package com.mlogger.handlers;

import com.mlogger.Log;
import com.mlogger.LogElement;
import com.utils.Utils;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Log4jHandler extends LogHandler {

    @Override
    public void publish(LogElement element, LinkedList<Handler> handlers, LogRecord record) throws Exception {

        if (!(element instanceof Log))
            return;

        Log log = (Log) element;

        switch (log.kind.value()) {
            case TRACE:
                getLogger(log).trace(createMessage(log));
                break;
            case DEBUG:
            case EVENT:
            case QUERY:
            case REQUEST:
                getLogger(log).trace(createMessage(log));
                break;
            case LOG:
            case INFO:
                getLogger(log).info(createMessage(log), log.getException());
                break;
            case WARNING:
            case EXCEPTION:
                getLogger(log).warn(createMessage(log), log.getException());
                break;
            case ERROR:
                getLogger(log).error(createMessage(log), log.getException());
                break;
        }

    }

    private static String createMessage(Log log) {

        StringBuilder sb = new StringBuilder();
        sb.append(log.value.isEmpty() ? "" : Utils.toString(log.value.value().value));

        if (log.sourceCode != null)
            if (log.sourceCode.value() != null) {
                sb.append("   [");
                sb.append(Utils.toString(log.sourceCode.value().fileName));
                sb.append(":");
                sb.append(Utils.toString(log.sourceCode.value().fileName));
                sb.append("]");
            }

        return sb.toString();

    }

    private static Logger getLogger(Log l) {
        StringBuilder sb = new StringBuilder();

        sb.append(l.source.value());

        String tag = l.tag.value().toString();
        if (tag.length() > 2) {
            sb.append(".");
            sb.append(tag);
        }

        String loggerName = sb.toString();

        Logger logger = LogManager.getLogger(loggerName);

        return logger;
    }

}

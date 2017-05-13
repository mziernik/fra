package com.mlogger.handlers;

import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import com.mlogger.Log;
import com.mlogger.LogElement;
import com.mlogger.storage.LogsStorage;

/**
 * Miłosz Ziernik 2014/06/13
 */
public class MLogFileHandler extends LogHandler {

    public MLogFileHandler() {
        super();
    }

    @Override
    public void publish(LogElement le, LinkedList<Handler> handlers, LogRecord record) throws Exception {
        if (!(le instanceof Log))
            return;

        Log log = (Log) le;
        LogsStorage.write(log);
    }

}

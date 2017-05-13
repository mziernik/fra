package com.mlogger;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Miłosz Ziernik 2013/05/29
 */
// handler, ktorego zadaniem jest przekierowanie logu do MLogger-a ( a nastepnie do pozostalych handlerow )
public class MHandler extends Handler {

    static MHandler handler;

    public final static byte[] signature = {(byte) 245, (byte) 253, (byte) 193, (byte) 250, (byte) 249};

    public final static ThreadGroup threadGroup = new ThreadGroup("Logger Threads");

    public static MHandler getInstance() {
        if (handler == null)
            handler = new MHandler();
        return handler;
    }

    private MHandler() {
        super();
        if (handler != null)
            return;

        handler = this;
        Logger.getGlobal().addHandler(this);
        Logger.getLogger("").addHandler(this);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void publish(LogRecord record) {
        MLogger.instance().log(record);
    }

    @Override
    public void flush() {
    }

    @Override
    public void close() throws SecurityException {

    }

}

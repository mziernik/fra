package com.mlogger.interfaces;

import java.io.IOException;
import java.util.logging.Handler;
import com.mlogger.LogElement;
import com.mlogger.handlers.LogHandler;

public class LogException extends IOException {

    public final Handler handler;
    public final LogElement log;

    public LogException(Handler handler, LogElement log, String message) {
        super(message);
        this.handler = handler;
        this.log = log;
    }

    public LogException(Handler handler, LogElement log, String message, Throwable cause) {
        super(message, cause);
        this.handler = handler;
        this.log = log;
    }

    public LogException(Handler handler, LogElement log, Throwable cause) {
        super(cause);
        this.handler = handler;
        this.log = log;
    }

}

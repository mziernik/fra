package com.mlogger.interfaces;

import java.util.logging.Handler;
import com.mlogger.Log;
import com.mlogger.LogElement;
import org.slf4j.impl.SLF4JLogger;

public interface ILogEventsHandler {

    public void onAfterCreate(Log log);

    public void onError(Log log, Throwable e);

    // public void onAfterCreate(Status status);
    // Handler = LogHanlder
    //  public boolean onBeforeSend(Handler handler, Status status);
    // Handler = LogHanlder
    public boolean onBeforeSend(Handler handler, Log log);

//    public boolean onBeforeAddStatistic(Statictic stat);
    public void onException(LogException e);

    public void onInfo(Handler handler, LogElement log, String tag, String info);

    //public void onStatusInterval(Status status);
    public boolean onSLF4JLog(SLF4JLogger slf, Log log);
}

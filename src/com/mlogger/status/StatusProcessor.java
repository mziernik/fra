package com.mlogger.status;

import com.context.AppContext;
import com.cron.TTimer;
import com.mlogger.console.Console;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;

public class StatusProcessor extends TTimer {

    public StatusProcessor() {
        super(new Interval(1, Unit.SECONDS));
    }

    @Override
    protected void run() throws Exception {
        if (AppContext.isInitialized())
            Console.updateStatus(StatusGroup.ROOT);
    }

}

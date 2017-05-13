package com.cron;

import com.config.CService;
import com.config.engine.ConfigNode;
import com.config.engine.field.CfBool;
import com.config.engine.interfaces.Cfg;
import com.context.AppContext;
import static com.lang.LConfig.*;

public class CCron extends ConfigNode {

    public CCron() {
        super(CService.class, "cron", CRON__CRON);
    }

    @Cfg
    public final static CfBool logEvents = new CfBool("log_events",
            CRON__LOG_EVENTS,
            true);

    @Cfg
    public final static CfBool active = new CfBool("enabled",
            ACTIVE,
            true);

    public static boolean logEvents() {
        return logEvents.value(AppContext.devMode || AppContext.testMode());
    }
}

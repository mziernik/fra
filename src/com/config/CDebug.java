package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.CfBool;
import com.config.engine.ValueSource;
import com.context.AppContext;
import static com.lang.LConfig.*;
import com.config.engine.interfaces.Cfg;

public class CDebug extends ConfigNode {

    public CDebug() {
        super(CService.class, "debug", DEBUG__DEBUG);
    }
    @Cfg
    public final static CfBool threadEvents = new CfBool("thread_events",
            DEBUG__THREAD_EVENTS, false);

    @Cfg
    public final static CfBool statisticsEnabled = new CfBool("statistics.enabled",
            DEBUG__STATISTICS_ENABLED, false)
            .onGetValue(CDebug.class, (CfBool item, Boolean value, ValueSource source)
                    -> value != null ? value : AppContext.devMode);

}

package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.CfBool;
import com.config.engine.field.CfBoolStringList;
import com.utils.collections.Pair;
import com.context.AppContext;
import com.mlogger.MLogger;
import com.mlogger.handlers.LogHandler;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.*;
import com.utils.collections.TList;
import java.util.logging.Handler;

public class CLogs extends ConfigNode {

    public CLogs() {
        super(CService.class, "logging", LOGS__LOGS);
        instance = this;
    }

    private static CLogs instance;

    @Cfg
    public final static CfBool javaScriptErrors = new CfBool("javascript_errors",
            LOGS__JAVASCRIPT_ERRORS,
            true);

    @Cfg
    public final static CfBool javaScript = new CfBool("javascript_messages",
            LOGS__JAVASCRIPT_MESSAGES,
            true);

    private final static TList<Handler> cfgHandlers = new TList<>();

    @Override
    protected void onInitialize() throws Exception {
        for (Handler h : AppContext.logger.getHandlers())
            if (cfgHandlers.contains(h))
                AppContext.logger.removeHandler(h);
        cfgHandlers.clear();

        for (Pair<Boolean, String> pair : protocols.value()) {
            if (pair.first == null || !pair.first)
                continue;

            LogHandler handler = MLogger.getHandlerFromUrl(pair.second);
            if (handler != null) {
                AppContext.logger.addHandler(handler);
                cfgHandlers.add(handler);
                handler.start();
            }
        }
    }

    @Cfg
    public final static CfBoolStringList protocols = new CfBoolStringList("protocols",
            LOGS__PROTOCOLS, ACTIVE, URL)
            .description(LOGS__PROTOCOLS__DESCRIPTION)
            .onAfterChange((item, isUserValue, newValue) -> {
                instance.onInitialize();
            });
}

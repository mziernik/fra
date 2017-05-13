package com.mlogger.console;

import com.config.CLogs;
import com.config.engine.ConfigNode;
import com.config.engine.field.CfBool;
import com.config.engine.field.CfInt;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.*;

public class CConsole extends ConfigNode {

    public CConsole() {
        super(CLogs.class, "console", CONSOLE__CONSOLE);
    }

    @Cfg
    public final static CfBool enabled = new CfBool("enabled",
            ENABLED,
            true);

    @Cfg
    public final static CfInt maxCount = new CfInt("max_count",
            CONSOLE__MAX_LOGS_COUNT,
            5_000);
}

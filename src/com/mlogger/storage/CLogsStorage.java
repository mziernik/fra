package com.mlogger.storage;

import com.config.CLogs;
import com.config.engine.ConfigNode;
import com.config.engine.field.*;
import com.config.engine.interfaces.Cfg;
import com.utils.Size;
import com.utils.Size.SizeUnit;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import static com.lang.LConfig.*;

public class CLogsStorage extends ConfigNode {

    public CLogsStorage() {
        super(CLogs.class, "storage", STORAGE__STORAGE);
    }

    @Cfg
    public final static CfBool enabled = new CfBool("enabled",
            ENABLED,
            false);
    /*
    @Cfg
    public final static CfString path = new CfString("storage.path",
            PATH,
            null).required(false);

    @Cfg
    public final static CfSize maxFileSize = new CfSize("max_file_size",
            STORAGE__MAX_FILE_SIZE,
            new Size(300, SizeUnit.MB));

    @Cfg
    public final static CfInterval suspendTime = new CfInterval("suspend_time",
            STORAGE__SUSPEND_TIME,
            new Interval(300, Unit.MILLISECONDS))
            .description(STORAGE__SUSPEND_TIME__DESCRIPTION);

    @Cfg
    public final static CfSize memoryBuferLimit = new CfSize("memory_bufer_limit",
            STORAGE__MEMORY_BUFER_LIMIT,
            new Size(10, SizeUnit.MB));

    @Cfg
    public final static CfInt daysRange = new CfInt("days_range",
            STORAGE__DAYS_RANGE,
            300)
            .description(STORAGE__DAYS_RANGE__DESCRIPTION);

    @Cfg
    public final static CfInt pageLogsLimit = new CfInt("page_logs_limit",
            STORAGE__PAGE_LOGS_LIMIT,
            300)
            .setup(self -> {
                self.cells[0].min(10);
                self.cells[0].max(10_000);
            });*/
}

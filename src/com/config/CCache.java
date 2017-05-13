package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.CfInterval;
import com.config.engine.field.CfSize;
import com.utils.Size;
import com.utils.Size.SizeUnit;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.*;

public class CCache extends ConfigNode {

    public CCache() {
        super(CService.class, "files_cache", CACHE__CACHE);
    }
    @Cfg
    public final static CfSize maxMemorySize = new CfSize("max_memory_size",
            CACHE__MAX_MEMORY_SIZE,
            new Size(1, SizeUnit.MB));

    @Cfg
    public final static CfInterval expireTime = new CfInterval("expire_time",
            CACHE__EXPIRE_TIME,
            new Interval(300, Unit.SECONDS));

}

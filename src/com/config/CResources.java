package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.CfBool;
import com.config.engine.field.CfInterval;
import com.config.engine.field.CfInt;
import com.config.engine.field.CfSize;
import com.config.engine.interfaces.*;
import com.context.AppContext;
import com.utils.Size;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import static com.lang.LConfig.*;

public class CResources extends ConfigNode {

    public CResources() {
        super(CService.class, "resources", RES__RES);
    }
    @Cfg
    public final static CfBool useCache = new CfBool("use_cache",
            RES__USE_CACHE,
            !AppContext.devMode);

    @Cfg
    public final static CfBool checkFileChanges = new CfBool("check_changes",
            RES__CHECK_CHANGES,
            AppContext.devMode);

    @Cfg
    public final static CfInterval expire = new CfInterval("expire",
            RES__EXPIRE,
            new Interval(24, Unit.HOURS))
            .description(RES__EXPIRE__DESCRIPTION);

    @Cfg
    public final static CfInt cacheSize = new CfInt("cache_size",
            RES__CACHE_SIZE,
            100);

    @Cfg
    public final static CfSize maxCachedItemSize = new CfSize(
            "max_cached_item_size",
            RES__MAX_CACHED_ITEM_SIZE,
            new Size(1, Size.SizeUnit.MB));

}

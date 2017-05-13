package com.cache.intf;

import com.cache.CachedData;

public interface CachedDataExpireListener {

    public boolean onCachedDataExpire(CachedData cache);

}

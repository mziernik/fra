package com.webapi;

import com.mlogger.storage.WLogsStorage;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;

public class WLogs implements WebApi {

    @WebApiEndpoint
    public final WLogsStorage storage = new WLogsStorage();

}

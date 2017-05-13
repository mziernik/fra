package com.webapi;

import com.cache.WCache;
import com.config.engine.HConfig;
import com.config.engine.WConfig;
import com.database.WDatabase;
import com.lang.core.WLanguage;
import com.service.cron.WCron;
import com.service.events.WEvents;
import com.servlet.interfaces.Arg;
import com.servlet.interfaces.Endpoint;
import com.servlet.websocket.WebSocketEndpoint;
import com.user.WUsers;
import com.model.dataset.AbstractDataSet;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiController;
import com.webapi.core.WebApiEndpoint;

@Endpoint(url = {"$/api", "$/api/*"})
@WebSocketEndpoint(url = "$api")
public class WApi extends WebApiController {

    @WebApiEndpoint
    public final WConfig config = new WConfig(HConfig.instance());

    @WebApiEndpoint
    public final WCron cron = new WCron();

    @WebApiEndpoint
    public final WUsers user = new WUsers();

    @WebApiEndpoint
    public final WEvents events = new WEvents();

    @WebApiEndpoint
    public final WSystem system = new WSystem();

    @WebApiEndpoint
    public final WTools tools = new WTools();

    @WebApiEndpoint
    public final WCache cache = new WCache();

    @WebApiEndpoint
    public final WHttp http = new WHttp();

    @WebApiEndpoint
    public final WebSocet webSocket = new WebSocet();

    @WebApiEndpoint
    public final WDatabase database = new WDatabase();

    @WebApiEndpoint
    public final WLanguage language = new WLanguage();

    @WebApiEndpoint
    public final WLogs log = new WLogs();

    @WebApiEndpoint
    public final WTest test = new WTest(this);
}

class WebSocet implements WebApi {

    @WebApiEndpoint()
    public AbstractDataSet sessions() {
        return null;
    }

    @WebApiEndpoint()
    public void disconnect(@Arg(name = "id") String id) {

    }

}

class WHttp implements WebApi {

    @WebApiEndpoint()
    public AbstractDataSet sessions() {
        return null;
    }

    @WebApiEndpoint()
    public void invalidateSession(@Arg(name = "id") String id) {

    }
}

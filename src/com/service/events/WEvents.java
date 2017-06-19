package com.service.events;

import com.context.EventsHandler;
import com.json.JArray;
import com.json.JObject;
import com.lang.LService;
import com.model.repository.Repository;
import com.service.events.filter.BaseEventsFilterHandler;
import com.servlet.Handlers;
import com.servlet.interfaces.Arg;
import com.utils.Utils;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;

public class WEvents implements WebApi {

    private BaseEventsFilterHandler filterHandler;

    @WebApiEndpoint
    public Repository filter(WebApiRequest request) throws Exception {
        JObject json = request.getJson().asObject();
        JArray filters = json.arrayD("filters");
        JObject params = json.objectD("parameters");

        BaseEventsFilterHandler fh = getFilterHandler();

        fh.filter(filters, params);
        Repository data = fh.getDataPage(1, 100);

//        if (data.results != null && data.results >= fh.resultLimit)
//            request.warningHint(LService.LIMITED_RESULT.toString(fh.resultLimit));
        return data;
    }

    @WebApiEndpoint
    public Repository result(@Arg(name = "offset", required = false) Integer offset) throws Exception {
        BaseEventsFilterHandler handler = getFilterHandler();
        offset = Utils.range(offset, 0, handler.resultsIds.size());
        int limit = 200;

        return handler.getData(offset, limit);
    }

    @WebApiEndpoint
    public JObject getInitData() {
        return getFilterHandler().toJson();
    }

    @WebApiEndpoint
    public JObject getDetails(@Arg(name = "id", nonEmpty = true) Integer id) throws Exception {
        return getFilterHandler().getDetails(id);
    }

    private BaseEventsFilterHandler getFilterHandler() {
        if (filterHandler != null)
            return filterHandler;

        EventsHandler instance = Handlers.events.getInstance();
        if (instance == null)
            throw new Error(LService.NO_EVENT_HANDLER_IMPLEMENTATION.toString());

        filterHandler = instance.getFilterHandler();
        if (filterHandler == null)
            throw new Error(LService.NO_EVENT_FILTER_IMPLEMENTATION.toString());

        return filterHandler;
    }
}

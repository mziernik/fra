package com.servlet.handlers.temporary;

import com.servlet.controller.Page;
import com.exceptions.http.Http400BadRequestException;
import com.exceptions.http.Http410GoneException;
import com.utils.Utils;
import com.utils.Is;
import com.html.core.Html;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.utils.Url;
import com.utils.collections.Params;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.util.*;
import java.util.Map.Entry;

/**
 * Miłosz Ziernik 2014/04/01
 */
//ToDo Przerobić na kontolera 
public abstract class TempRequest {

    public final int expiresSeconds;
    public final String eTag = "E'" + Utils.randomId(10);
    public final String id;
    public final String name;
    public boolean removeAfterDownload = false;
    public final TDate created = new TDate();
    public final HashMap<String, Object> data = new HashMap<>();
    public final Map<String, Object> extra = new HashMap<>();

    private final static LinkedList<TempRequest> requests = new LinkedList<>();  // sync
    private final static LinkedList<String> gones = new LinkedList<>();  // sync
    public final Object source;

    public TempRequest(String name, Interval expire) {
        this(name, expire, expire);
    }

    public TempRequest(String name, Interval expire, Object source) {
        this.expiresSeconds = (int) expire.getTime(Unit.SECONDS);
        this.id = Utils.randomId(20);
        Utils.checkId(name, true, ".");
        this.name = name;
        this.source = source;
        synchronized (requests) {
            requests.add(this);
        }
    }

    public Url getUrl() {
        return new Url("/" + name).param(id);
    }

    protected abstract Class<? extends Page> getHandler(HttpRequest request) throws Exception;

    public void remove() {
        synchronized (requests) {
            requests.remove(this);
        }
    }

    public static TempRequest getByName(String name) {
        synchronized (requests) {
            return Utils.findFirst(requests, (TempRequest tr) -> name.equals(tr.name));
        }
    }

    public static TempRequest getByExtra(String name, Object value) {
        synchronized (requests) {
            for (TempRequest req : requests)
                for (Entry<String, Object> en : req.extra.entrySet())
                    if (Objects.equals(name, en.getKey()) && Objects.equals(value, en.getValue()))
                        return req;
        }
        return null;
    }

    public static Class<? extends Page> processRequest(HttpRequest request) throws Exception {
        // ------------------------ TempRequest -------------------------
        Params.Param param = request.url.params().first();

        if (param == null || !Is.empty(param.value))
            return null;

        String id = param.name;

        synchronized (requests) {

            synchronized (gones) {
                Set<TempRequest> toRemove = new HashSet<>();

                for (TempRequest req : requests)
                    if (req.created.isExpired(req.expiresSeconds * 1000)) {
                        toRemove.add(req);
                        gones.add(req.id);
                    }

                requests.removeAll(toRemove);

                while (requests.size() > 10000)
                    gones.add(requests.pollFirst().id);

                while (gones.size() > 1000)
                    gones.remove(0);

            }

            if (!id.isEmpty())
                for (TempRequest req : requests)
                    if (req.id.equals(id)) {
                        request.properties.putAll(req.data);
                        if (req.removeAfterDownload)
                            req.remove();
                        return req.getHandler(request);
                    }

            synchronized (gones) {
                if (!id.isEmpty())
                    if (gones.contains(id))
                        throw new Http410GoneException();
            }

        }
        return null;
        // ------------------------ / TempRequest -------------------------}
    }

    @Endpoint
    public static class TempRequestPage extends Page {

        @Override
        public void onRequest(HttpRequest http) throws Exception {
            Html html = (Html) request.properties.get("$req_html");
            if (html == null)
                throw new Http400BadRequestException(request);
            returnHTML(html);
        }
    }

}

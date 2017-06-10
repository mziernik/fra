package com.webapi;

import com.exceptions.ServiceException;
import com.json.*;
import com.lang.core.LString;
import com.mlogger.Log;
import com.servlet.interfaces.Arg;
import com.utils.collections.*;
import com.webapi.core.DataType_old;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.regex.Pattern;

public class WNotifications implements WebApi {

    public final SyncList<String> currentHash = new SyncList<String>()
            .sizeLimit(100, OverloadAction.SHIFT);

    public final static SyncMap<String, LString> notifySources = new SyncMap<>();
    public final MapList<String, Pattern> notifyPatterns = new MapList<>();

    @WebApiEndpoint(dataType = DataType_old.OBJECT)
    public void register(WebApiRequest request, 
            
            @Arg(name = "sources")String[] sourStrings ) {
        JObject json = request.getJson().asObject();
        MapList<String, Pattern> map = new MapList<>();

        for (JElement el : json) {
            String src = el.getName();

            if (!notifySources.containsKey(src))
                throw new ServiceException("Unknown notify source: " + src);

            if (el.isValue())
                map.add(src, Pattern.compile(el.asString()));

            if (el.isArray())
                for (JValue val : el.asArray().getValues())
                    map.add(src, Pattern.compile(val.asString()));

        }

        synchronized (notifyPatterns) {
            notifyPatterns.clear();
            notifyPatterns.addAll(map);
        }
    }

    @WebApiEndpoint
    public JObject getSources() {
        JObject json = new JObject();
        for (Entry<String, LString> en : notifySources.entrySet())
            json.put(en.getKey(), en.getValue().toString());
        return json;
    }

    @WebApiEndpoint
    public void hashChange(@Arg(name = "hash") String hash) {
        currentHash.add(hash);
    }

    @WebApiEndpoint
    public JObject getPatterns() {
        JObject json = new JObject();
        for (Entry<String, LinkedList<Pattern>> en : notifyPatterns) {
            JArray arr = json.arrayC(en.getKey());
            for (Pattern p : en.getValue())
                arr.add(p.toString());
        }
        return json;
    }

    @WebApiEndpoint(dataType = DataType_old.OBJECT)
    public void log(WebApiRequest req,
            @Arg(name = "type") String type,
            @Arg(name = "value") String value) {

        switch (type.toLowerCase()) {
            case "error":
                Log.error("JS", value);
                break;
            default:
                Log.debug("JS", value);
                break;
        }

    }
}

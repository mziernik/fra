package com.servlet.websocket;

import com.exceptions.EError;
import com.mlogger.Log;
import com.json.JObject;
import com.json.JSON;
import java.io.*;

/**
 * Miłosz Ziernik 2013/11/16 * Wymagany tomcat > 7.0.45
 */
public abstract class JsonWebSocket extends WebSocketController {

    protected String name;
    private String currentAction = null;
    private String currentUid = null;

    @Override
    public void onError(Throwable thr) {
        super.onError(thr);
    }

    protected abstract void onMessage(String action, JObject in) throws Exception;

    @Override
    public void onMessage(String msg) {
        JObject in = JSON.parse(msg).asObject();
        try {
            currentUid = in.getStr("uid");
            currentAction = in.getStr("action");
            onMessage(currentAction, in.objectD("data"));
        } catch (Throwable e) {
            onError(e);
            Log.warning("JsonWebSocket", e);
            write(e);
        }
    }

    public void write(JObject json) throws IOException {

        JObject src = new JObject();
        src.put("action", currentAction);
        src.put("uid", currentUid);
        src.put("data", json);

        json.options.quotaNames(true)
                .acceptNulls(true)
                .intent("")
                .singleLine(true);
        send(src.toString());
    }

    public void write(Throwable e) {
        JObject json = new JObject();
        json.options.quotaNames(true)
                .acceptNulls(true)
                .intent("")
                .singleLine(true);
        json.put("action", currentAction);
        json.put("uid", currentUid);
        json.put("exception", EError.toString(e));
        send(json.toString());
    }

}

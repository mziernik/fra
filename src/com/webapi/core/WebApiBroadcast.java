package com.webapi.core;

import com.mlogger.Log;
import com.mlogger.LogKind;
import com.servlet.websocket.WebSocketConnection;
import com.servlet.websocket.WebSocketController;
import com.utils.collections.TList;
import static com.webapi.core.WebApiController.buildEvent;

public class WebApiBroadcast {

    public final String source;
    public final String event;
    public final Object data;

    public final TList<WebApiController> recipients = new TList<>();

    public WebApiBroadcast(String source, Object data) {
        this(source, null, data);
    }

    public WebApiBroadcast(String source, String event, Object data) {
        this.source = source;
        this.event = event;
        this.data = data;

        for (WebApiController ctrl : WebSocketConnection.getControllers(WebApiController.class))
            if (ctrl.canSendEvent(source, event, data))
                recipients.add(ctrl);
    }

    public void send() {
        if (recipients.isEmpty())
            return;
        String str = buildEvent(null, source, event, data).toString();

        new Log(LogKind.TRACE)
                .tag("WebApi|Broadcast")
                .value(source + ", " + event)
                .details(data)
                .send();

        for (WebSocketController ctrl : recipients)
            ctrl.send(str);
    }

}

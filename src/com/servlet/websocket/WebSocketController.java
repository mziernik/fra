package com.servlet.websocket;

import com.config.CService;
import com.context.AppContext;
import com.events.EventListeners;
import com.intf.runnable.Runnable1;
import com.lang.core.Language;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.thread.ThreadObject;
import com.utils.TObject;
import java.io.IOException;
import javax.websocket.CloseReason;

public abstract class WebSocketController {

    private transient boolean closed;
    public final WebSocketConnection connection;
    public final EventListeners<Runnable1<CloseReason>> onClose = new EventListeners<>();
    public final TObject<Language> language = new TObject<>(CService.language.value())
            .notNull(true);

    public WebSocketController() {
        this.connection = ThreadObject.wsConn.get();
        if (connection != null && connection.httpSession != null)
            language.set(connection.httpSession.language.get());
        language.onChange((sender, currentValue, newValue) -> {
            ThreadObject.language.set(newValue);
            return true;
        });
    }

    public abstract void onMessage(String message);

    public void onMessage(byte[] message) {

    }

    public void broadcast(String data) {
        if (closed)
            return;

        for (WebSocketController ctrl : WebSocketConnection.getControllers(getClass()))
            ctrl.send(data);
    }

    public synchronized void send(String data) {
        if (closed)
            return;
        try {
            connection.send(data);
        } catch (Throwable e) {
            onError(e);
        }
    }

    public synchronized void send(byte[] data) {
        if (closed)
            return;
        try {
            connection.send(data);
        } catch (Throwable e) {
            onError(e);
        }
    }

    public void onError(Throwable e) {
        Log.warning(e);
    }

    public void close() throws IOException {
        closed = true;
        connection.close();
    }

    public void onClose(CloseReason reason) {
        closed = true;
        connection.onClose(reason);
        if (AppContext.devMode)
            new Log(LogKind.EVENT)
                    .tag("WebSocket")
                    .value("Zakończenie sesji " + connection.requestUrl)
                    .address(connection.remoteAddress.getHostString())
                    .user(connection.httpSession != null ? connection.httpSession.user.username : null)
                    .send();

        for (Runnable1<CloseReason> r : onClose)
            r.run(reason);
    }
}

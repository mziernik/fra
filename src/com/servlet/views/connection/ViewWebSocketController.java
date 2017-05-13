package com.servlet.views.connection;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppContext;
import com.json.JArray;
import com.json.JObject;
import com.json.JSON;
import com.lang.LServlet;
import com.servlet.interfaces.JsMethod;
import com.servlet.views.ViewController;
import com.servlet.views.ViewControllerMeta;
import com.servlet.websocket.WebSocketController;
import com.thread.ThreadObject;
import com.utils.reflections.TClass;
import javax.websocket.CloseReason;

public class ViewWebSocketController extends WebSocketController {

    public final ViewController ctrl;

    public ViewWebSocketController(ViewControllerMeta viewMeta) {
        ThreadObject.viewWebSocketController.set(this);
        ctrl = new TClass<>(viewMeta.view).newInstance(null);
    }

    @Override
    public void onError(Throwable e) {
        ctrl.onError(e);
    }

    @Override
    public void onMessage(String message) {
        JObject in = JSON.parse(message).asObject();
        try {

            if (in.has("#exception#")) {
                JObject jexc = in.object("#exception#");

                String msg = jexc.getStr("message", null);
                if (!Is.empty(msg))
                    ctrl.onJsError(
                            jexc.getStr("url", ""),
                            jexc.getStr("title", ""),
                            msg,
                            jexc.getStr("stack", "").split("\n"));
                return;
            }

            for (JArray arr : in.getArrays()) {
                String funct = arr.getName();

                for (JsMethod.JsMethodImpl method : ctrl.meta.jsMethods)
                    if (method.name.equals(funct)) {
                        method.method.invoke(ctrl, arr);
                        return;
                    }

                if (!AppContext.releaseMode())
                    throw new RuntimeException(
                            LServlet.METHOD_NOT_FOUND.toString(this.getClass().getName(), funct));
            }

        } catch (Throwable e) {
            ctrl.onError(e);
        }
    }

    @Override
    public void onClose(CloseReason reason) {
        super.onClose(reason);
        ctrl.onClose(reason);
    }

}

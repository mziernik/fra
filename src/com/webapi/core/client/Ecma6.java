package com.webapi.core.client;

import com.config.CHttp;
import com.context.AppConfig;
import com.json.Escape;
import com.json.JArray;
import com.json.JObject;
import com.servlet.interfaces.Arg;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.servlet.websocket.WebSocketEndpoint;
import com.utils.Is;
import com.utils.Url;
import com.utils.collections.Strings;
import com.utils.reflections.TMethod;
import com.utils.text.NameFormat;
import com.utils.text.StrWriter;
import com.webapi.core.DataType;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiController;
import com.webapi.core.WebApiControllerMeta;
import java.util.LinkedList;

/**
 *
 * @author milosz
 */
public class Ecma6 extends WebApiClientBuilder {

    public Ecma6(WebApiController controller, StrWriter writer) {
        super(controller, writer);
    }

    @Override
    public void build() {

        Class<? extends WebApiController> cls = controller.getClass();
        Endpoint httpEndp = cls.getAnnotation(Endpoint.class);
        WebSocketEndpoint wsEndp = cls.getAnnotation(WebSocketEndpoint.class);

        String wsUrl = null;
        if (wsEndp != null) {
            wsUrl = CHttp.url((Url) null, wsEndp.url(), null).toString();
            wsUrl = wsUrl.replace("http://", "ws://");
        }

        String httpUrl = null;
        if (httpEndp != null) {
            httpUrl = CHttp.url((Url) null, httpEndp.url()[0], null).toString();
            if (httpUrl.endsWith("/"))
                httpUrl = httpUrl.substring(0, httpUrl.length() - 1);
        }

        //   writer.setLevel(1);
        javascriptClient(httpUrl, wsUrl, controller.getClass(), null, true);
    }

    @Override
    public void build(HttpRequest http, Class<? extends WebApi> cls, String parent) {

        WebSocketEndpoint ws = controller.getClass().getAnnotation(WebSocketEndpoint.class);

        String wsUrl = CHttp.url(http, ws.url(), null).toString();
        wsUrl = wsUrl.replace("http://", "ws://");

        String httpUrl = CHttp.url(http, http.relativePath, null).toString();
        if (httpUrl.endsWith("/"))
            httpUrl = httpUrl.substring(0, httpUrl.length() - 1);

        if (parent != null && httpUrl.endsWith("/" + parent))
            httpUrl = httpUrl.substring(0, httpUrl.length() - parent.length() - 1);

        javascriptClient(httpUrl, wsUrl, cls, parent, true);
    }

    private void javascriptClient(String httpUrl, String wsUrl,
            Class<? extends WebApi> cls, String parent, boolean root) {

        if (Is.empty(parent))
            parent = null;

        if (root) {
            writer.append("// @flow\n"
                    + "'use strict';\n"
                    + "\n"
                    + "import WebApi from \"../core/webapi/WebApi\";\n"
                    + "import WebApiRequest from \"../core/webapi/Request\";\n"
                    + "import WebApiResponse from \"../core/webapi/Response\";\n"
                    + "\n"
                    + "type OnSuccess = (data: ?any, response: WebApiResponse) => void;\n"
                    + "type OnError = (error: Object, response: WebApiResponse) => void;\n"
                    + "\n"
                    + "export default class ")
                    .append(new NameFormat().firstUpper().camelCase().format(AppConfig.getServiceName()))
                    .append("Api {")
                    .lineBreak().lineBreak();

            writer.setLevel(writer.getLevel() + 1);

            writer.intent()
                    .append("api: WebApi;")
                    .lineBreak()
                    .lineBreak()
                    .intent()
                    .append("constructor(api: WebApi) {")
                    .lineBreak()
                    .nextLevel(() -> {
                        writer.intent().append("this.api = api;").lineBreak();

                        writer.intent().append("api.httpUrl = api.httpUrl || \"")
                                .append(httpUrl)
                                .append("\";")
                                .lineBreak();

                        writer.intent().append("api.wsUrl = api.wsUrl || \"")
                                .append(wsUrl)
                                .append("\";")
                                .lineBreak();

                        writer.intent().append("api.hash = '")
                                .append(controller.getHash())
                                .append("';")
                                .lineBreak();
                    }).lineBreak().intent().append("}");

        }

        LinkedList<WebApiControllerMeta> list = WebApiControllerMeta.map.get(cls);

        list.sort((WebApiControllerMeta o1, WebApiControllerMeta o2) -> o1.name.compareTo(o2.name));

        boolean first = true;
        for (WebApiControllerMeta m : list)
            if (m.returnWebApi != null) {

                if (!first && !root)
                    writer.append(",");

                first = false;

                writer.lineBreak().intent();

                if (root)
                    writer.append(m.name).append(": Object = {");
                else
                    writer.append(m.name).append(": {");

                if (!Is.empty(m.endp.description()))
                    writer.append(" /** ").append(Escape.unquoted(m.endp.description())).append(" */");

                String sparrent = parent;
                writer.nextLevel(() -> {
                    javascriptClient(httpUrl, wsUrl, m.returnWebApi,
                            (Is.empty(sparrent) ? "" : sparrent + "/") + m.name, false);
                });

                writer.lineBreak().intent().append("}");
                if (root)
                    writer.append(";").lineBreak();
            }

        for (WebApiControllerMeta m : list)
            if (m.returnWebApi == null) {

                if (!first && !root)
                    writer.append(",");

                first = false;

                writer.lineBreak().intent();

                if (!Is.empty(m.endp.description()))
                    writer.append("/** ").append(Escape.unquoted(m.endp.description())).append(" */")
                            .lineBreak()
                            .intent();

                writer.append(m.name).append(root ? ": Object = " : ": ").append("(");
                boolean hasParams = buildParams(m);
                writer.append("onSuccess: ?OnSuccess = null, onError: ?OnError = null): WebApiRequest => ");
//                }

                writer.lineBreak().intent(writer.getLevel() + 1);

                writer.append("this.api.call(\"")
                        .append(new Strings(parent, m.name).toString("/"))
                        .append("\", '")
                        .append(m.hash)
                        .append("', ")
                        .append(hasParams ? "params" : "null")
                        .append(", onSuccess, onError)");

                writer.lineBreak().intent();
                if (root)
                    writer.append(";").lineBreak();

                //    visit(m.returnWebApi, path + m.name + "/", level + 1, ul);
            }

        if (root)
            writer.lineBreak()
                    .intent()
                    //                    .append("api.initImpl(this);")
                    //                    .lineBreak()
                    //                    .lineBreak()
                    .append("}")
                    .setLevel(writer.getLevel() - 1);
    }

    private boolean buildParams(WebApiControllerMeta m) {
        JObject json = new JObject();
        json.options.quotaNames(false);

        if (m.item instanceof TMethod) {

            TMethod mth = (TMethod) m.item;
            for (Arg.ArgMeta a : mth.arguments) {

                if (a.ann == null)
                    continue;

                String clsName = a.cls.raw.getSimpleName().toLowerCase();
                if (a.cls.raw == Object.class)
                    clsName = "any";

                if (Number.class.isAssignableFrom(a.cls.raw))
                    clsName = "number";

                if (a.cls.raw == JObject.class)
                    clsName = "Object";

                if (a.cls.raw == JArray.class)
                    clsName = "Array";

                if (!a.required)
                    clsName = "?" + clsName;

                json.putRaw(a.name, clsName);
            }
        }

        if (json.isEmpty())
            return false;

        writer.append("params: ");
        json.options.singleLine(true);
        json.getContent(writer);
        writer.append(", ");

        return true;

    }

}

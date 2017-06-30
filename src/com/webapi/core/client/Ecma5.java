/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
import com.utils.collections.TList;
import com.utils.reflections.TMethod;
import com.utils.text.NameFormat;
import com.utils.text.StrWriter;
import com.webapi.core.DataType_old;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiController;
import com.webapi.core.WebApiControllerMeta;
import java.util.LinkedList;

/**
 *
 * @author milosz
 */
public class Ecma5 extends WebApiClientBuilder {

    public Ecma5(WebApiController controller, StrWriter writer) {
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
            writer.append("function ")
                    .append(new NameFormat().firstUpper().camelCase().format(AppConfig.getServiceName()))
                    .append("Api(api) {")
                    .br();

            writer.setLevel(writer.getLevel() + 1);

            writer.intent()
                    .append("\"use strict\";")
                    .br()
                    .br()
                    .intent()
                    .append("this.api = api;")
                    .br();

            writer.intent().append("api.httpUrl = api.httpUrl || \"")
                    .append(httpUrl)
                    .append("\";")
                    .br();

            writer.intent().append("api.wsUrl = api.wsUrl || \"")
                    .append(wsUrl)
                    .append("\";")
                    .br();

            writer.intent().append("api.hash = '")
                    .append(controller.getHash())
                    .append("';")
                    .br();

        }

        TList<WebApiControllerMeta> list = WebApiControllerMeta.map.get(cls);

        list.sort((WebApiControllerMeta o1, WebApiControllerMeta o2) -> o1.name.compareTo(o2.name));

        boolean first = true;
        for (WebApiControllerMeta m : list)
            if (m.returnWebApi != null) {

                if (!first && !root)
                    writer.append(",");

                first = false;

                writer.br().intent();

                if (root)
                    writer.append("this.").append(m.name).append(" = {");
                else
                    writer.append(m.name).append(": {");

                if (!Is.empty(m.endp.description()))
                    writer.append(" // ").append(Escape.unquoted(m.endp.description()));

                String sparrent = parent;
                writer.nextLevel(() -> {
                    javascriptClient(httpUrl, wsUrl, m.returnWebApi,
                            (Is.empty(sparrent) ? "" : sparrent + "/") + m.name, false);
                });

                writer.br().intent().append("}");
                if (root)
                    writer.append(";").br();
            }

        for (WebApiControllerMeta m : list)
            if (m.returnWebApi == null) {

                if (!first && !root)
                    writer.append(",");

                first = false;

                writer.br().intent();

                if (root)
                    writer.append("this.").append(m.name).append(" = ")
                            .append("function (data) {");
                else
                    writer.append(m.name).append(": function (data) {");

                if (!Is.empty(m.endp.description()))
                    writer.append(" // ").append(Escape.unquoted(m.endp.description()));

                JObject json = new JObject();
                json.options.quotaNames(false);

                if (m.item instanceof TMethod) {

                    if (m.endp.dataType() != DataType_old.NONE)
                        json.arrayC("")
                                .add(m.endp.dataType().name().toLowerCase())
                                .add(true).options.quotaNames(true);

                    TMethod mth = (TMethod) m.item;
                    for (Arg.ArgMeta a : mth.arguments) {

                        if (a.ann == null)
                            continue;

                        String clsName = a.cls.raw.getSimpleName().toLowerCase();
                        if (a.cls.raw == Object.class)
                            clsName = null;

                        if (Number.class.isAssignableFrom(a.cls.raw))
                            clsName = "number";

                        if (a.cls.raw == JObject.class)
                            clsName = "object";

                        if (a.cls.raw == JArray.class)
                            clsName = "array";

                        json.arrayC(a.name).add(clsName).add(a.required);
                    }
                }

                String flags = "CRUD";

                writer.br()
                        .intent(writer.getLevel() + 1)
                        .append("return api.call(\"")
                        .append(new Strings(parent, m.name).toString("/"))
                        .append("\", '")
                        .append(m.hash)
                        .append("', '")
                        .append(flags)
                        .append("', data, ");

                writer.nextLevel(() -> {
                    json.getContent(writer);
                });

                writer.append(");");

                writer.br().intent().append("}");
                if (root)
                    writer.append(";").br();

                //    visit(m.returnWebApi, path + m.name + "/", level + 1, ul);
            }

        if (root)
            writer.br()
                    .intent()
                    .append("api.initImpl(this);")
                    .br()
                    .br()
                    .append("}")
                    .setLevel(writer.getLevel() - 1);
    }

}

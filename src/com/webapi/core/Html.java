package com.webapi.core;

import com.cache.CachedData;
import com.config.CHttp;
import com.context.AppConfig;
import com.exceptions.http.Http400BadRequestParamException;
import com.html.core.dict.BoxSizing;
import com.html.core.dict.Flex;
import com.html.core.dict.FlexDirection;
import com.html.core.styles.*;
import com.html.core.tag.Body;
import com.html.core.tag.form.Select;
import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.semantic.Span;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.html.js.Call;
import com.json.Escape;
import com.json.JArray;
import com.json.JObject;
import com.resources.Res;
import com.resources.core.Resources;
import com.servlet.interfaces.Arg;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.servlet.websocket.WebSocketEndpoint;
import com.utils.Char;
import com.utils.Url;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.reflections.TMethod;
import com.utils.text.StrWriter;
import java.io.IOException;
import java.util.LinkedList;

//ToDo: Dodać funkkcję łączenia parametrów z różnych gałęzi (jak w lincall trunk/config 0 trunkID + configID
// ToDo: Wyświetlać kategorie w formie drzewa
class Html {

    static void printInfo(WebApiController controller, HttpRequest http,
            Class<? extends WebApi> cls, String methodName) throws Exception {

        if (".clinet".equals(http.params.firstName())) {
            getClient(controller, http, Utils.toString(http.params.first().value), cls, methodName);
            return;
        }

        com.html.core.Html html = new com.html.core.Html(controller);

        Body body = html.body;

        body.style().fontFamilyMonospace();

        html.head.styles("li > div")
                .display(Display.inlineFlex);
        html.head.styles("li span")
                .display(Display.inlineFlex);

        html.head.styles(".comment")
                .marginLeft("20px")
                .color("#666")
                .fontStyle("italic")
                .fontSize("8pt");

        html.head.styles(".return-type")
                .marginRight("8px")
                .fontStyle("italic")
                .color("#666");

        html.head.styles(".defValue")
                .marginLeft("6px")
                .color("#666")
                .fontStyle("italic");

        body.h3("WebApi, " + AppConfig.getServiceTitle()
                + (Is.empty(methodName) ? "" : ", " + methodName))
                .style()
                .paddingLeft("2em");

        body.hr();

        Ul ul = body.ul();
        ul.style()
                .listStyleType("none")
                .padding("10px")
                .lineHeight("1.5em")
                .fontSize("10pt");

        LinkedList<WebApiControllerMeta> list = WebApiControllerMeta.map.get(cls);

        list.sort((WebApiControllerMeta o1, WebApiControllerMeta o2) -> o1.name.compareTo(o2.name));

        for (WebApiControllerMeta m : list)
            if (m.returnWebApi != null) {
                Div div = ul.li().div();

                div.a(m.name)
                        .href("/" + controller.http().relativePath + "/" + m.name)
                        .style()
                        .fontWeight(FontWeight.bold);

                div.span(m.endp.description()).cls("comment");
                //    visit(m.returnWebApi, path + m.name + "/", level + 1, ul);
            }

        for (WebApiControllerMeta m : list)
            if (m.returnWebApi == null) {
                Div div = ul.li().div();

                Class<?> returnClass = m.item.getReturnType();
                String className = CachedData.class.isAssignableFrom(returnClass)
                        ? "File"
                        : m.item.getReturnType().getSimpleName();

                div.span(className)
                        .cls("return-type");

                div.a(m.name)
                        .href("/" + controller.http().relativePath + "/" + m.name + "?.tester")
                        .style()
                        .fontWeight(FontWeight.bold)
                        .marginRight("8px");

                Span liArgs = div.span();

                if (m.item instanceof TMethod)
                    for (Arg.ArgMeta am : ((TMethod) m.item).arguments) {

                        if (am.cls.raw == WebApiRequest.class)
                            continue;

                        liArgs.span(liArgs.isEmpty() ? "(" : "," + Char.nbsp);

                        if (am.required && am.defaultValue == null)
                            liArgs.span("*");

                        liArgs.span(am.shortTypeName + " " + am.name);

                        if (am.defaultValue != null)
                            liArgs.span("[" + am.defaultValue + "]")
                                    .cls("defValue")
                                    .title("Wartość domyślna");

                    }
                if (liArgs.isEmpty())
                    liArgs.span("(");
                liArgs.span(")");

                Strings comment = new Strings().nonEmpty(true);
                if (m.endp.dataType() != DataType.NONE)
                    comment.add("[" + m.endp.dataType().name().toLowerCase() + "]");

                if (m.endp.rights().length > 0)
                    comment.add("[" + Utils.toString(m.endp.rights()) + "]");

                comment.add(m.endp.description());
                div.span(comment.toString(" ")).cls("comment");

            }
        body.hr();

        Div d = body.div();
        d.style().fontSize("0.8em").paddingLeft("10px");
        d.a("webapi.js").href("?.clinet=javascript");

        http.returnHTML(html, 200);
    }

    static void getClient(WebApiController controller, HttpRequest http, String client, Class<? extends WebApi> cls, String endpoint) {

        StrWriter writer = new StrWriter();
        writer.setIntent("\t");

        switch (client) {
            case "javascript":
                javascriptClient(controller, http, writer, cls, endpoint);
                http.contentDisposition.inline = true;
                http.contentDisposition.setHeader("webapi.js");
                http.returnCustom(writer.toString(), "application/javascript");
                return;
        }

        throw new UnsupportedOperationException(client);
    }

    public static void buildJavascriptClient(WebApiController controller, StrWriter writer) {

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
        javascriptClient(controller, httpUrl, wsUrl, writer, controller.getClass(), null, true);
    }

    static void javascriptClient(WebApiController controller, HttpRequest http,
            StrWriter writer, Class<? extends WebApi> cls, String parent) {

        WebSocketEndpoint ws = controller.getClass().getAnnotation(WebSocketEndpoint.class);

        String wsUrl = CHttp.url(http, ws.url(), null).toString();
        wsUrl = wsUrl.replace("http://", "ws://");

        String httpUrl = CHttp.url(http, http.relativePath, null).toString();
        if (httpUrl.endsWith("/"))
            httpUrl = httpUrl.substring(0, httpUrl.length() - 1);

        if (parent != null && httpUrl.endsWith("/" + parent))
            httpUrl = httpUrl.substring(0, httpUrl.length() - parent.length() - 1);

        javascriptClient(controller, httpUrl, wsUrl, writer, cls, parent, true);
    }

    private static void javascriptClient(WebApiController controller, String httpUrl, String wsUrl,
            StrWriter writer, Class<? extends WebApi> cls, String parent, boolean root) {

        if (Is.empty(parent))
            parent = null;

        if (root) {
            writer.append("function ")
                    .append(Character.toUpperCase(AppConfig.getServiceName().charAt(0)))
                    .append(AppConfig.getServiceName().substring(1))
                    .append("Api(api) {")
                    .lineBreak();

            writer.setLevel(writer.getLevel() + 1);

            writer.intent()
                    .append("\"use strict\";")
                    .lineBreak()
                    .lineBreak()
                    .intent()
                    .append("this.api = api;")
                    .lineBreak();

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
                    writer.append("this.").append(m.name).append(" = {");
                else
                    writer.append(m.name).append(": {");

                if (!Is.empty(m.endp.description()))
                    writer.append(" // ").append(Escape.unquoted(m.endp.description()));

                String sparrent = parent;
                writer.nextLevel(() -> {
                    javascriptClient(controller, httpUrl, wsUrl, writer, m.returnWebApi,
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

                    if (m.endp.dataType() != DataType.NONE)
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

                writer.lineBreak()
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

                writer.lineBreak().intent().append("}");
                if (root)
                    writer.append(";").lineBreak();

                //    visit(m.returnWebApi, path + m.name + "/", level + 1, ul);
            }

        if (root)
            writer.lineBreak()
                    .intent()
                    .append("api.initImpl(this);")
                    .lineBreak()
                    .lineBreak()
                    .append("}")
                    .setLevel(writer.getLevel() - 1);
    }

    static void tester(WebApiController controller, HttpRequest http, WebApiRequest req)
            throws Http400BadRequestParamException, IOException {

        com.html.core.Html html = new com.html.core.Html(controller);

        html.head.link(Res.ace);
        html.head.script().innerHTML(Resources.getStr(Html.class, "tester.js"));

        //----------------------------------------------------------------------
        html.head.styles("body")
                .fontFamilySans()
                .fontSize("10pt")
                .padding("0")
                .margin("0")
                .backgroundColor("#fafafa")
                .position(Position.absolute)
                .boxSizing(BoxSizing.borderBox)
                .display(Display.flex)
                .flexDirection(FlexDirection.column)
                .width("100%")
                .height("100%");

        html.head.styles("input")
                .border("none")
                .padding("1px 4px")
                .margin("0")
                .fontFamilyMonospace();

        html.head.styles(".ace_content")
                .padding("8px");

        html.head.styles(".ace_editor")
                .fontSize("11pt")
                .fontFamilyMonospace();

        html.head.styles("h4")
                .color("#444")
                .margin("6px");

        html.head.styles("h3")
                .margin("6px");

        html.head.styles("td")
                .border("1px solid #ccc")
                .padding("0");

        html.head.styles("iframe")
                .flex(Flex.auto)
                .border("none")
                .borderTop("1px solid #ccc")
                .width("100%")
                .height("100%");

        //----------------------------------------------------------------------
        Div main = html.body.div();
        main.style().padding("8px");

        main.h3("Endpoint " + req.endpointName);

        Div container = main.div();
        container.style().display(Display.flex);

        {
            Div left = container.div();

            left.h4("Parametry:");

            Table tbl = left.table().id("tblParams");

            tbl.style().borderSpacing("0").borderCollapse("collapse");

            int cnt = 0;

            if (req.meta.item instanceof TMethod) {
                TMethod mth = (TMethod) req.meta.item;
                for (Arg.ArgMeta a : mth.arguments) {
                    if (a.ann == null)
                        continue;
                    ++cnt;
                    Tr tr = tbl.tbodyTr();
                    tr.td().inputText().value(a.name);
                    tr.td().inputText().value(a.defaultValue);
                }

                for (; cnt < 10; cnt++) {
                    Tr tr = tbl.tbodyTr();
                    tr.td().inputText();
                    tr.td().inputText();
                }

            }

            left.br();

            left.h4("Rezultat:").style()
                    .float_(SFloat.left)
                    .margin("0")
                    .marginTop("30px");

            left.button("Wyślij")
                    .onClick(new Call("send"))
                    .style()
                    .padding("8px 16px")
                    .float_(SFloat.right);
        }

        {
            Div right = container.div();
            right.style()
                    .display(Display.flex)
                    .paddingLeft("10px")
                    .paddingRight("10px")
                    .flexDirection(FlexDirection.column)
                    .flex(Flex.auto);
            right.h4("Dane (JSON):");

            Div ta = right.div().id("ta-data");

            ta.style()
                    .width("100%")
                    .height("100%")
                    .border("1px solid #ccc");;

            if (req.meta.endp.dataType() == DataType.ARRAY)
                ta.text("[\n\n]");

            if (req.meta.endp.dataType() == DataType.OBJECT)
                ta.text("{\n\n}");
        }

        Url url = new Url(req.http.url);
        url.params().clear();

        main.inputHidden().id("endpointUrl").value(url.toString());
        main.inputHidden().id("endpointName").value(req.endpointName);

        html.body.iframe().name("result");

    }

}

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
import com.utils.collections.TList;
import com.utils.reflections.TMethod;
import com.utils.text.StrWriter;
import com.webapi.core.client.Ecma5;
import com.webapi.core.client.Ecma6;
import com.webapi.core.client.Repositories;
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

        TList<WebApiControllerMeta> list = WebApiControllerMeta.map.get(cls);

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
                if (m.endp.dataType() != DataType_old.NONE)
                    comment.add("[" + m.endp.dataType().name().toLowerCase() + "]");

                if (m.endp.rights().length > 0)
                    comment.add("[" + Utils.toString(m.endp.rights()) + "]");

                comment.add(m.endp.description());
                div.span(comment.toString(" ")).cls("comment");

            }
        body.hr();

        Div d = body.div();
        d.style().fontSize("0.8em").paddingLeft("10px");
        d.a("ecma5.js").href("?.clinet=ecma5");
        d.a("ecma6.js").href("?.clinet=ecma6");
        d.a("repositories.js").href("?.clinet=repositories");
        http.returnHTML(html, 200);
    }

    static void getClient(WebApiController controller, HttpRequest http, String client, Class<? extends WebApi> cls, String endpoint) {

        StrWriter writer = new StrWriter();
        writer.setIntent("\t");

        switch (client) {
            case "ecma5":
                new Ecma5(controller, writer).build(http, cls, endpoint);
                http.contentDisposition.inline = true;
                http.contentDisposition.setHeader("webapi.js");
                http.returnCustom(writer.toString(), "application/javascript");
                return;
            case "ecma6":
                new Ecma6(controller, writer).build(http, cls, endpoint);
                http.contentDisposition.inline = true;
                http.contentDisposition.setHeader("webapi.js");
                http.returnCustom(writer.toString(), "application/javascript");
                return;
            case "repositories":
                new Repositories(controller, writer).build(http, cls, endpoint);
                http.contentDisposition.inline = true;
                http.contentDisposition.setHeader("Repositories.js");
                http.returnCustom(writer.toString(), "application/javascript");
                return;
        }

        throw new UnsupportedOperationException(client);
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

            if (req.meta.endp.dataType() == DataType_old.ARRAY)
                ta.text("[\n\n]");

            if (req.meta.endp.dataType() == DataType_old.OBJECT)
                ta.text("{\n\n}");
        }

        Url url = new Url(req.http.url);
        url.params().clear();

        main.inputHidden().id("endpointUrl").value(url.toString());
        main.inputHidden().id("endpointName").value(req.endpointName);

        html.body.iframe().name("result");

    }

}

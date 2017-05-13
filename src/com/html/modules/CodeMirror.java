package com.html.modules;

import com.html.core.tag.Element;
import com.html.core.tag.form.TextArea;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.programming.Script;
import com.json.JObject;
import com.resources.Res;
import com.servlet.controller.ControllerEndpoint;
import com.servlet.controller.Page;
import com.servlet.controller.intf.BeforeReturnHtml;
import com.utils.reflections.TMethod;

public class CodeMirror extends TextArea {

    private final Page page;
    private final String jsVariableName;
    private final CodemirrorMode mode;

    public static enum CodemirrorMode {

        javaScript("javascript", "javascript.js", "application/javascript"),
        xml("html/xml", "xml.js", "text/html"),
        mixed("mixed", null, "text/plain");

        final String name;
        final String file;
        final String contentType;

        private CodemirrorMode(String name, String file, String contentType) {
            this.name = name;
            this.file = file;
            this.contentType = contentType;
        }

    }

    public String sendAjaxAction(String url, String onResponse) {
        return "$id('" + getId(true) + "').sendAjax('" + url + "', "
                + (onResponse != null ? "function(http){" + onResponse + "}"
                        : "null") + ")";
    }

    public CodeMirror(Page page, Parent parent, CodemirrorMode mode, String jsVariableName) {
        super(parent);
        this.page = page;
        this.jsVariableName = jsVariableName;
        this.mode = mode;
        page.endpoint().beforeReturnHtml.add((BeforeReturnHtml) this::onBeforeReturnHtml);
    }

    private boolean onBeforeReturnHtml(ControllerEndpoint<?> ctrl, Element tag, int status) {

        page.link(Res.utils);

        page.link(
                "/res/codemirror/codemirror.js",
                "/res/codemirror/codemirror.css",
                "/res/codemirror/closebrackets.js"
        );

        if (mode.file != null)
            page.link("/res/codemirror/" + mode.file);

        String id = getId(true);

        JObject obj = new JObject();
        obj.options.javascriptMode(true);
        obj.put("mode", mode.name);
        obj.put("lineNumbers", true);
        obj.put("matchBrackets", true);

        JObject keys = obj.objectC("extraKeys");
        keys.putRaw("Ctrl-S",
                "function(instance) {\n"
                + "  alert(instance.getValue());\n"
                + "}"
        );
        keys.put("Ctrl-Q", "toggleComment");

        keys.put("Ctrl.Alt.Del", "toggleComment");

        obj.put("continueComments", "Enter");
        obj.put("autoCloseBrackets", true);

        new Script((Parent) getParent()).text(
                "var " + jsVariableName
                + " = CodeMirror.fromTextArea($id('" + id + "'), "
                + obj.toString()
                + ");\n\n"
                + "$id('" + id + "').sendAjax = function(url, onResponse){\n"
                + "  ajax.post(url, {\n"
                + "    post: " + jsVariableName + ".getValue(),\n"
                + "    contentType: '" + mode.contentType + "'\n"
                + "}, onResponse);\n"
                + "};"
        );
        /*
         parentNode().script(
         "var " + jsVariableName
         + " = CodeMirror.fromTextArea(document.getElementById('" + id + "'), {\n"
         + " mode: \"" + mode.name + "\",\n"
         + " lineNumbers : true,\n"
         + " matchBrackets : true,\n"
         + " extraKeys: {\n"
         + " \"Ctrl-S\": function(instance) {\n"
         + " alert(instance.getValue());\n"
         + " },\n"
         + " \"Ctrl-Q\" : \"toggleComment\"\n"
         + " },\n"
         + " continueComments : \"Enter\",\n"
         + " autoCloseBrackets : true"
         + "});\n");
         */
        return true;
    }

}

package com.html.js;

import com.html.core.tag.Element;
import com.html.js.core.JsAction;
import com.json.JCollection;
import com.resources.Res;
import com.utils.text.StrWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class JQuery extends JsAction {

    private final String selector;
    private final List<Call> calls = new LinkedList<>();

    public JQuery(String selector) {

        this.selector = selector;
    }

    public JQuery prop(String name, Object value) {
        calls.add(new Call("prop", name, value));
        return this;
    }

    public JQuery val() {
        calls.add(new Call("val"));
        return this;
    }

    public JQuery css(String style, Object value) {
        calls.add(new Call("css", style, value));
        return this;
    }

    public JQuery text(Object value) {
        calls.add(new Call("text", value));
        return this;
    }

    public JQuery html(Object value) {
        calls.add(new Call("html", value));
        return this;
    }

    public JQuery attr(String name, Object value) {
        calls.add(new Call("prop", name, value));
        return this;
    }

    public JQuery removeAttr(String name) {
        calls.add(new Call("removeAttr", name));
        return this;
    }

    @Override
    public JQuery setTag(Element node) {
        link(node, Res.jQuery);
        super.setTag(node);
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {

        writer.append("$(");
        escape(writer, selector).append(")");

        for (Call call : calls) {
            writer.append(".");
            call.getContent(writer);
        }
    }

}

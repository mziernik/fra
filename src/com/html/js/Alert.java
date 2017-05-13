package com.html.js;

import com.html.core.tag.Element;
import com.html.js.core.JsAction;
import com.resources.Res;

public class Alert extends Call {

    public Alert(Object message, Object text) {
        super("$alert", message, text);
    }

    public Alert(Object message) {
        this(message, null);
    }

    @Override
    public JsAction setTag(Element node) {
        link(node, Res.utils);
        super.setTag(node);
        return this;
    }

}

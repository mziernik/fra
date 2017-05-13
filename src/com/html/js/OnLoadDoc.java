package com.html.js;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.js.core.JsAction;

public class OnLoadDoc extends JsAction {

    private final JsActions actions = new JsActions();

    public OnLoadDoc(JsAction... actions) {
        this.actions.add(actions);
    }

    @Override
    public void getContent(StrWriter writer) {
        if (actions.isEmpty())
            return;
        writer.append("window.addEventListener('load', ");
        new Function(null).body(actions).getContent(writer);
        writer.append(")");
    }

    @Override
    public JsAction setTag(Element tag) {
        actions.setTag(tag);
        return this;
    }

}

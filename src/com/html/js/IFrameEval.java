package com.html.js;

import com.html.js.core.JsAction;
import com.utils.text.StrWriter;
import com.html.core.tag.Element;

public class IFrameEval extends JsAction {

    final JsActions actions;
    final String frameId;

    public IFrameEval(String frameId, JsAction... actions) {
        this.frameId = frameId;
        this.actions = new JsActions(actions);
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("frames['").append(frameId).append("'].contentWindow.eval(");
        escape(writer, actions.toString());
        writer.append(")");
    }

    @Override
    public JsAction setTag(Element node) {
        actions.setTag(node);
        return this;
    }

}

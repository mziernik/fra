package com.html.js;

import com.html.js.core.JsAction;
import com.html.js.core.AbstractUrlAction;
import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.resources.Res;
import com.servlet.controller.Controller;
import com.utils.Url;

@Deprecated
public class Layer extends AbstractUrlAction<Layer> {

    private final JsActions onClose = new JsActions();

    private String varName = "newLayer";

    public Layer(Class<? extends Controller> page) {
        super(new Url(page));
    }

    public Layer(Controller page) {
        super(new Url(page));
    }

    public Layer(String url) {
        super(url);
    }

    public Layer onClose(JsAction... onClose) {
        this.onClose.add(onClose);
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("var ")
                .append(varName)
                .append(" = new top.Layer('")
                .append(getUrl().toString())
                .append("');");

        if (onClose != null) {
            writer.append("\n")
                    .append(varName)
                    .append(".onClose = function(){ ");
            onClose.getContent(writer);
            writer.append(" }");
        }
    }

    @Override
    public Layer setTag(Element tag) {
        super.setTag(tag);
        link(tag, Res.layer);
        if (tag == null)
            return this;

        if (onClose != null)
            onClose.setTag(tag);
        return this;
    }

}

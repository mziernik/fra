package com.html.js;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.js.core.AbstractUrlAction;
import com.html.js.core.JsAction;
import com.servlet.controller.Controller;
import com.utils.Url;

//ToDo do zrobienia
public class PopupWindow extends AbstractUrlAction {

    public PopupWindow(Class<? extends Controller> page) {
        super(new Url(page));
    }

    public PopupWindow(Controller page) {
        super(new Url(page));
    }

    public PopupWindow(String url) {
        super(url);
    }

    public PopupWindow width(double value, boolean percentage) {
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("showPopupWindow('")
                .append(getUrl().toString())
                .append(", 640, 480)");
    }

    @Override
    public JsAction setTag(Element tag) {
        return super.setTag(tag);
    }

}

package com.html.js;

import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.html.core.tag.Element;
import com.html.js.core.JsAction;
import com.resources.Res;

public class Prompt extends JsAction {

    private final JsActions onConfirm;
    private final String varName;
    private final Object title;
    private final Object defValue;

    public Prompt(String varName, Object title, Object defValue, JsAction... onConfirm) {
        Utils.checkId(Utils.toString(varName), true);
        this.onConfirm = new JsActions(onConfirm);
        this.varName = varName;
        this.title = title;
        this.defValue = defValue;
    }

    @Override
    public void getContent(StrWriter writer) {
        new Call("$prompt", title, defValue,
                new Function(null, varName)
                        .body(onConfirm))
                .getContent(writer);
    }

    @Override
    public JsAction setTag(Element tag) {
        onConfirm.setTag(tag);
        link(tag, Res.utils);
        return this;
    }

}

package com.html.core.tag.programming;

import com.html.core.tag.form.Form;
import com.html.core.tag.form.FormTag;
import com.html.core.tag.intfs.*;
import com.utils.Url;

public class TObject extends FormTag<TObject> implements Visual<TObject>, InnerText<TObject> {

    public TObject(Parent parent) {
        super(parent, "object");
        requiredAttribute("width", "height", "data");
    }

    @Override
    public TObject data(Url data) {
        return attrs.setHref("data", data).tag;
    }

    public Url dataB(String data) {
        return attrs.setHrefB("data", data);
    }

    @Override
    public TObject form(Form form) {
        return super.form(form);
    }

    @Override
    public TObject width(Number width) {
        return super.width(width);
    }

    @Override
    public TObject height(Number height) {
        return super.height(height);
    }

    public TObject type(String type) {
        return attrs.set("type", type).tag;
    }

}

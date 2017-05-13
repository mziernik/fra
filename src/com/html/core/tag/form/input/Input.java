//Ta klasa dotyczy tylko tagów input z atrybutem type.
package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.form.DataList;
import com.html.core.tag.form.Form;
import com.html.core.tag.form.FormTag;
import com.html.core.tag.intfs.Closed;
import com.html.core.tag.intfs.Parent;

public class Input<TTag extends Input> extends FormTag<TTag> implements Closed {

    public Input(Parent parent, InputType type) {
        super(parent, "input");
        type(type);
        requiredAttribute("type");
    }

    public TTag type(InputType type) {
        return attrs.setEnum("type", type).tag;
    }

    @Override
    public TTag form(Form form) {
        return super.form(form);
    }

    @Override
    protected TTag value(Object value) {
        return super.value(value);
    }

    @Override
    protected TTag readOnly(boolean readonly) {
        return super.readOnly(readonly);
    }

    @Override
    protected TTag required(boolean required) {
        return super.required(required);
    }

    protected DataList dataList() {
        return new DataList((Parent) parent, this);
    }

}

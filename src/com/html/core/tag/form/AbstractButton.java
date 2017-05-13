package com.html.core.tag.form;

import com.html.core.dict.*;
import com.html.core.tag.intfs.*;
import com.utils.Url;

public abstract class AbstractButton<TTag extends AbstractButton> extends FormTag<TTag>
        implements Visual<TTag>, Container<TTag> {

    public AbstractButton(Parent parent) {
        super(parent, "button");
    }

    @Override
    public TTag autofocus(boolean autofocus) {
        return super.autofocus(autofocus);
    }

    @Override
    public TTag formAction(Url formaction) {
        return super.formAction(formaction);
    }

    @Override
    public TTag formEncType(EnctypeType formenEnctypeType) {
        return super.formEncType(formenEnctypeType);
    }

    @Override
    public TTag formMethod(MethodType formMethodType) {
        return super.formMethod(formMethodType);
    }

    @Override
    public TTag formNoValidate(String formNoValidate) {
        return super.formNoValidate(formNoValidate);
    }

    @Override
    public TTag formTarget(TargetType formTargetType) {
        return super.formTarget(formTargetType);
    }

    public TTag type(ButtonType type) {
        return attrs.setEnum("type", type).tag;
    }

    @Override
    public TTag value(Object value) {
        return super.value(value);
    }

    @Override
    public TTag form(Form form) {
        return super.form(form);
    }

}

package com.html.core.tag.form.input;

import com.html.core.dict.*;
import com.html.core.tag.form.Form;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;
import com.utils.Url;

public class InputSubmit extends Input<InputSubmit> implements Visual<InputSubmit> {

    public InputSubmit(Parent parent) {
        super(parent, InputType.submit);
    }

    @Override
    public InputSubmit formAction(Url formaction) {
        return super.formAction(formaction);
    }

    @Override
    public InputSubmit formEncType(EnctypeType formenEnctypeType) {
        return super.formEncType(formenEnctypeType);
    }

    @Override
    public InputSubmit formMethod(MethodType formMethodType) {
        return super.formMethod(formMethodType);
    }

    @Override
    public InputSubmit formNoValidate(String formNoValidate) {
        return super.formNoValidate(formNoValidate);
    }

    @Override
    public InputSubmit formTarget(TargetType formTargetType) {
        return super.formTarget(formTargetType);
    }

    @Override
    public InputSubmit value(Object value) {
        return super.value(value);
    }

}

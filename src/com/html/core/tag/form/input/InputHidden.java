package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;

public class InputHidden extends Input<InputHidden> {

    public InputHidden(Parent parent) {
        super(parent, InputType.hidden);
    }

    @Override
    public InputHidden value(Object value) {
        return super.value(value);
    }

}

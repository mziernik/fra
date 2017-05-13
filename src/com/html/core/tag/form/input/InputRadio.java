package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;

public class InputRadio extends CheckableInput<InputRadio> {

    public InputRadio(Parent parent) {
        super(parent, InputType.radio);
    }

    @Override
    public InputRadio value(Object value) {
        return super.value(value);
    }

    @Override
    public InputRadio required(boolean required) {
        return super.required(required);
    }

}

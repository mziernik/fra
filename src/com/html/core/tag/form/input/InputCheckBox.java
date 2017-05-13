package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class InputCheckBox extends CheckableInput<InputCheckBox>
        implements Visual<InputCheckBox> {

    public InputCheckBox(Parent parent) {
        super(parent, InputType.checkbox);
    }

    @Override
    public InputCheckBox value(Object value) {
        return super.value(value);
    }

    @Override
    public InputCheckBox required(boolean required) {
        return super.required(required);
    }

}

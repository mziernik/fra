package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class InputButton extends Input<InputButton>
        implements Visual<InputButton> {

    public InputButton(Parent parent) {
        super(parent, InputType.button);
    }

    @Override
    public InputButton value(Object value) {
        return super.value(value);
    }
}

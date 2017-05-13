package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class InputNumber extends Input<InputNumber>
        implements Visual<InputNumber> {

    public InputNumber(Parent parent) {
        super(parent, InputType.number
        );
    }

    @Override
    public InputNumber autocomplete(Boolean autocomplete) {
        return super.autocomplete(autocomplete);
    }

    public InputNumber max(Number max) {
        return attrs.setNumber("max", max).tag;
    }

    public InputNumber min(Number min) {
        return attrs.setNumber("min", min).tag;
    }

    @Override
    public InputNumber placeholder(String placeholder) {
        return super.placeholder(placeholder);
    }

    @Override
    public InputNumber readOnly(boolean readonly) {
        return super.readOnly(readonly);
    }

    @Override
    public InputNumber required(boolean required) {
        return super.required(required);
    }

    @Override
    public InputNumber value(Object value) {
        return super.value(value);
    }

}

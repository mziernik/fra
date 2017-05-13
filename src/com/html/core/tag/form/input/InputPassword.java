package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class InputPassword extends Input<InputPassword>
        implements Visual<InputPassword> {

    public InputPassword(Parent parent) {
        super(parent, InputType.password);
    }

    @Override
    public InputPassword autocomplete(Boolean autocomplete) {
        return super.autocomplete(autocomplete);
    }

    @Override
    public InputPassword placeholder(String placeholder) {
        return super.placeholder(placeholder);
    }

    @Override
    public InputPassword readOnly(boolean readonly) {
        return super.readOnly(readonly);
    }

    @Override
    public InputPassword required(boolean required) {
        return super.required(required);
    }

    @Override
    public InputPassword size(Integer size) {
        return super.size(size);
    }

    @Override
    public InputPassword autofocus(boolean autofocus) {
        return super.autofocus(autofocus);
    }

    @Override
    public InputPassword value(Object value) {
        return super.value(value);
    }

}

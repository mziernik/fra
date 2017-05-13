package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;

public class InputSearch extends Input<InputSearch> {

    public InputSearch(Parent parent) {
        super(parent, InputType.search);
    }

    public InputSearch autosave(String autosaveId) {
        return attrs.set("autosave", autosaveId).tag;
    }

    @Override
    public InputSearch autocomplete(Boolean autocomplete) {
        return super.autocomplete(autocomplete);
    }

    @Override
    public InputSearch readOnly(boolean readonly) {
        return super.readOnly(readonly);
    }

    @Override
    public InputSearch required(boolean required) {
        return super.required(required);
    }

    @Override
    public InputSearch placeholder(String placeholder) {
        return super.placeholder(placeholder);
    }

    @Override
    public InputSearch size(Integer size) {
        return super.size(size);
    }

    @Override
    public InputSearch value(Object value) {
        return super.value(value);
    }

}

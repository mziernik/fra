package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.form.DataList;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class InputText extends Input<InputText>
        implements Visual<InputText> {

    public InputText(Parent parent) {
        super(parent, InputType.text);
    }

    @Override
    public InputText autofocus(boolean autofocus) {
        return super.autofocus(autofocus);
    }

    @Override
    public InputText autocomplete(Boolean autocomplete) {
        return super.autocomplete(autocomplete);
    }

    @Override
    public InputText required(boolean required) {
        return super.required(required);
    }

    @Override
    public InputText placeholder(String placeholder) {
        return super.placeholder(placeholder);
    }

    @Override
    public InputText readOnly(boolean readonly) {
        return super.readOnly(readonly);
    }

    @Override
    public InputText size(Integer size) {
        return super.size(size);
    }

    @Override
    public Label labelAfter(Object caption) {
        return super.labelAfter(caption);
    }

    @Override
    public Label labelBefore(Object caption) {
        return super.labelBefore(caption);
    }

    @Override
    public InputText list(DataList list) {
        return super.list(list);
    }

    @Override
    public DataList dataList() {
        return super.dataList();
    }

    @Override
    public InputText value(Object value) {
        return super.value(value);
    }

}

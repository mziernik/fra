package com.html.core.tag.form;

import com.html.core.dict.WrapType;
import com.html.core.tag.form.input.Label;
import com.html.core.tag.intfs.*;

/*
 Contains

 Text
 */
public class TextArea extends FormTag<TextArea>
        implements Visual<TextArea>, InnerText<TextArea> {

    public TextArea(Parent parent) {
        super(parent, "textarea");
    }

    @Override
    public TextArea autofocus(boolean autofocus) {
        return super.autofocus(autofocus);
    }

    public TextArea maxLength(Integer maxLength) {
        return attrs.setNumber("maxlength", maxLength).tag;
    }

    @Override
    public TextArea form(Form form) {
        return super.form(form);
    }

    @Override
    public TextArea placeholder(String placeholder) {
        return super.placeholder(placeholder);
    }

    @Override
    public TextArea readOnly(boolean readonly) {
        return super.readOnly(readonly);
    }

    @Override
    public TextArea required(boolean required) {
        return super.required(required);
    }

    public TextArea rows(Integer rows) {
        return attrs.setNumber("rows", rows).tag;
    }

    public TextArea cols(Integer cols) {
        return attrs.setNumber("cols", cols).tag;
    }

    public TextArea wrap(WrapType wrapType) {
        return attrs.setEnum("wrap", wrapType).tag;
    }

    @Override
    public Label labelAfter(Object caption) {
        return super.labelAfter(caption); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Label labelBefore(Object caption) {
        return super.labelBefore(caption); //To change body of generated methods, choose Tools | Templates.
    }

}

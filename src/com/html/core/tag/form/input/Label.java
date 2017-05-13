package com.html.core.tag.form.input;

import com.html.core.tag.Tag;
import com.html.core.tag.form.Form;
import com.html.core.tag.form.FormTag;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;

public class Label extends CTag<Label> {

    public Label(Tag<? extends Parent> parent, FormTag input) {
        super(parent, "label");
        requiredAttribute("for");
        if (input != null)
            for_(input);
    }

    private FormTag input;

    public Label for_(FormTag input) {
        this.input = input;
        input.properties.put("#label", this);
        return attrs.setId("for", input).tag;
    }

    @Override
    protected Label form(Form form) {
        return super.form(form);
    }

    public FormTag getInput() {
        return input;
    }

}

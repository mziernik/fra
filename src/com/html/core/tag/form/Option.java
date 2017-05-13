package com.html.core.tag.form;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.InnerText;
import com.html.core.tag.intfs.Parent;

/*
 Contains

 Text
 */
public class Option extends Element<Option> implements InnerText<Option> {

    public Option(Parent parent) {
        super(parent, "option");
    }

    @Override
    public Option disabled(boolean disabled) {
        return super.disabled(disabled);
    }

    public Option selected(boolean selected) {
        return attrs.setState("selected", selected).tag;
    }

    @Override
    public Option value(Object value) {
        return super.value(value);
    }

    /**
     * Specifies a shorter label for an option
     *
     * @param label
     * @return
     */
    public Option label(String label) {
        return attrs.set("label", label).tag;
    }

}

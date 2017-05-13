package com.html.core.tag.form;

import com.html.core.tag.Element;
import com.html.core.*;
import com.html.core.tag.form.input.Label;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

/*
 Contains

 At least one of the following elements must appear:

 optgroup/*
 Contains

 At least one of the following elements must appear:

 optgroup
 option
 */
public class Select extends FormTag<Select>
        implements Parent<Select>, Visual<Select> {

    public Select(Parent parent) {
        super(parent, "select");
    }

    public Option option() {
        return new Option(this);
    }

    public Option option(Object text, String value) {
        return new Option(this).value(value).text(text);
    }

    public OptGroup optGroup(String label) {
        return new OptGroup(this, label);
    }

    @Override
    public Select autofocus(boolean autofocus) {
        return super.autofocus(autofocus);
    }

    @Override
    public Select form(Form form) {
        return super.form(form);
    }

    public Select multiple(boolean multiple) {
        return attrs.setState("multiple", multiple).tag;
    }

    @Override
    public Select required(boolean required) {
        return super.required(required);
    }

    @Override
    public Label labelAfter(Object caption) {
        return super.labelAfter(caption);
    }

    @Override
    public Label labelBefore(Object caption) {
        return super.labelBefore(caption);
    }

    /**
     * Defines the number of visible options in a drop-down list
     *
     * @param size
     * @return
     */
    @Override
    public Select size(Integer size) {
        return super.size(size);
    }

    public class OptGroup extends Element<OptGroup> implements Parent<OptGroup> {

        public OptGroup(Select parent, String label) {
            super((Parent) parent, "optgroup");
            label(label);
        }

        @Override
        public OptGroup disabled(boolean disabled) {
            return super.disabled(disabled);
        }

        public final OptGroup label(String label) {
            return attrs.set("label", label).tag;
        }

        public Option option() {
            return new Option(this);
        }

        public OptGroup option(String value, Object text) {
            new Option(this).value(value).text(text);
            return self;
        }

    }

}

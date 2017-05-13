package com.html.core.tag.form;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.*;

public class Fieldset extends FormTag<Fieldset>
        implements Parent<Fieldset>, Visual<Fieldset>, Container<Fieldset> {

    public final Legend legend;

    public Fieldset(Parent parent, Object label) {
        super(parent, "fieldset");
        legend = new Legend(this).text(label);
    }

    @Override
    public Fieldset form(Form form) {
        return super.form(form);
    }

    public class Legend extends Element<Legend>
            implements Visual<Legend>, InnerText<Legend> {

        public Legend(Parent parent) {
            super(parent, "legend");
        }

    }
}

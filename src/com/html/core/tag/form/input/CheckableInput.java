//Ta klasa dotyczy tylko tagów input z atrybutem type.
package com.html.core.tag.form.input;

import com.html.core.dict.InputType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public abstract class CheckableInput<TTag extends CheckableInput>
        extends Input<TTag>
        implements Visual<TTag> {

    public CheckableInput(Parent parent, InputType type) {
        super(parent, type);
    }

    @Override
    public TTag checked(boolean checked) {
        return super.checked(checked);
    }

    @Override
    public Label labelAfter(Object caption) {
        return super.labelAfter(caption);
    }

    @Override
    public Label labelBefore(Object caption) {
        return super.labelBefore(caption); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public TTag value(Object value) {
        return super.value(value);
    }

    @Override
    public TTag name(String name) {
        return super.name(name);
    }

}

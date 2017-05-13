package com.html.core.tag.form.input;

import com.html.core.dict.*;
import com.html.core.tag.intfs.Parent;
import com.utils.Url;

public class InputImage extends Input<InputImage> {

    public InputImage(Parent parent) {
        super(parent, InputType.image);
        requiredAttribute("src", "alt");
    }

    @Override
    public InputImage alt(String alt) {
        return super.alt(alt);
    }

    @Override
    public InputImage formAction(Url formaction) {
        return super.formAction(formaction);
    }

    @Override
    public InputImage formEncType(EnctypeType formenEnctypeType) {
        return super.formEncType(formenEnctypeType);
    }

    @Override
    public InputImage formMethod(MethodType formMethodType) {
        return super.formMethod(formMethodType);
    }

    @Override
    public InputImage formNoValidate(String formNoValidate) {
        return super.formNoValidate(formNoValidate);
    }

    @Override
    public InputImage formTarget(TargetType formTargetType) {
        return super.formTarget(formTargetType);
    }

    @Override
    public InputImage width(Number width) {
        return super.width(width);
    }

    @Override
    public InputImage height(Number height) {
        return super.height(height);
    }

    @Override
    public InputImage src(Url src) {
        return super.src(src);
    }

}

package com.html.core.tag.formatting;

import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;

/**
 * The <meter> tag defines a scalar measurement within a known range, or a
 * fractional value. This is also known as a gauge.
 *
 * @author user
 */
public class Progress extends CTag<Progress> {

    public Progress(Parent parent) {
        super(parent, "progress");
    }

    public Progress max(Number max) {
        return attrs.setNumber("max", max).tag;
    }

    public Progress value(Number value) {
        return attrs.setNumber("value", value).tag;
    }
}

package com.html.core.tag.formatting;

import com.html.core.*;
import com.html.core.tag.form.Form;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;

/**
 * The <meter> tag defines a scalar measurement within a known range, or a
 * fractional value. This is also known as a gauge.
 *
 * @author user
 */
public class Meter extends CTag<Meter> {

    public Meter(Parent parent) {
        super(parent, "meter");
    }

    public Meter high(Number high) {
        return attrs.setNumber("high", high).tag;
    }

    public Meter low(Number low) {
        return attrs.setNumber("low", low).tag;
    }

    public Meter max(Number max) {
        return attrs.setNumber("max", max).tag;
    }

    public Meter min(Number min) {
        return attrs.setNumber("min", min).tag;
    }

    public Meter optimum(Number optimum) {
        return attrs.setNumber("optimum", optimum).tag;
    }

    public Meter value(Number value) {
        return attrs.setNumber("value", value).tag;
    }
}

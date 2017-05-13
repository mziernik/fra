package com.html.core.tag.list;

import com.html.core.tag.semantic.CTag;

public class Li extends CTag<Li> {

    public Li(Ul parent) {
        super(parent, "li");
    }

    public Li(Ol parent) {
        super(parent, "li");
    }

    @Override
    public Li value(Object value) {
        return super.value(value);
    }

}

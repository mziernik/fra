package com.html.core.tag.list;

import com.html.core.tag.Element;
import com.html.core.tag.Tag;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class Ul extends Element<Ul> implements Parent<Ul>, Visual<Ul> {

    public Ul(Parent parent) {
        super(parent, "ul");
    }

    public Li li() {
        return new Li(this);
    }

    public Li li(Object innerText) {
        return new Li(this).text(innerText);
    }

    public Ul addIems(Iterable<? extends Object> list) {
        if (list == null)
            return this;

        for (Object o : list)
            if (o != null)
                li().text(o);
        return this;
    }
}

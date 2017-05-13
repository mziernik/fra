package com.html.core.tag.meta;

import com.html.core.tag.Element;
import com.html.core.tag.Head;
import com.html.core.tag.intfs.Closed;
import com.utils.Url;

public class Base extends Element<Base> implements Closed {

    public Base(Head parent) {
        super(parent, "base");
        requiredAttribute("href");
    }

    @Override
    public Base href(Url href) {
        return super.href(href);
    }

    @Override
    public Url hrefB(String href) {
        return super.hrefB(href);
    }

    @Override
    public final Base target(String framename) {
        return super.target(framename);
    }

    @Override
    public Head getParent() {
        return (Head) parent;
    }
}

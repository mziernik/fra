package com.html.core.tag.meta;

import com.html.core.tag.Element;
import com.html.core.dict.*;
import com.html.core.tag.Head;
import com.html.core.tag.intfs.Closed;
import com.html.core.tag.intfs.Parent;
import com.utils.Url;

public class Link extends Element<Link> implements Closed {

    public Link(Parent parent) {
        super(parent, "link");
        requiredAttribute("href");
    }

    public Link rel(LinkType rel) {
        return attrs.setEnum("rel", rel).tag;
    }

    public Link type(String type) {
        return attrs.set("type", type).tag;
    }

    /**
     * Specifies how the element handles cross-origin requests
     *
     * @param crossorigin
     * @return
     */
    public Link crossorigin(Crossorigin crossorigin) {
        return attrs.setEnum("crossorigin", crossorigin).tag;
    }

    // <editor-fold defaultstate="collapsed" desc="Metody atrybutów">
    @Override
    public Link href(Url href) {
        return super.href(href);
    }

    @Override
    public Url hrefB(String href) {
        return super.hrefB(href);
    }

    public Link media(MediaType media) {
        return attrs.setEnum("media", media).tag;
    }

    @Override
    public Head getParent() {
        return (Head) parent;
    }

}

package com.html.core.tag.meta;

import com.html.core.tag.Element;
import com.html.core.dict.HttpEquiv;
import com.html.core.tag.Head;
import com.html.core.tag.intfs.Closed;
import com.html.core.tag.intfs.Parent;
import java.nio.charset.Charset;

public class Meta extends Element<Meta> implements Closed {

    public Meta(Head parent) {
        super(parent, "meta");
        requiredAttribute("name", "content");
    }

    @Override
    public Head getParent() {
        return (Head) parent;
    }

    /**
     * Specifies the character encoding for the HTML document
     *
     * @param charset
     * @return
     */
    public Meta charset(Charset charset) {
        return attrs.set("charset", charset != null ? charset.name() : null).tag;
    }

    /*
     Ten atrybut występuje tylko dla Meta.
     */
    public Meta content(String content) {
        attrs.set("content", content);
        return self;
    }

    /**
     * Provides an HTTP header for the information/value of the content
     * attribute
     *
     * @param httpEquiv
     * @return
     */
    public Meta httpEquiv(HttpEquiv httpEquiv) {
        attrs.setEnum("http-equiv", httpEquiv);
        return self;
    }

    public Meta name(String name) {
        return attrs.set("name", name).tag;
    }

}

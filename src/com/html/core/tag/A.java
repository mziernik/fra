package com.html.core.tag;

import com.html.core.dict.*;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;
import com.servlet.controller.Controller;
import com.utils.Url;

/**
 * The <a> tag defines a hyperlink, which is used to link from one page to
 * another.
 *
 * The most important attribute of the <a> element is the href attribute, which
 * indicates the link's destination.
 *
 * By default, links will appear as follows in all browsers:
 *
 * An unvisited link is underlined and blue A visited link is underlined and
 * purple An active link is underlined and red
 */
public class A extends CTag<A> {

    public A(Parent parent) {
        super(parent, "a");
    }

    @Override
    public A href(Url href) {
        return super.href(href);
    }

    public A href(String href) {
        return href(new Url(href));
    }

    @Override
    public Url hrefB(String href) {
        return super.hrefB(href);
    }

    @Override
    public Url hrefB(Class<? extends Controller> page) {
        return super.hrefB(page);
    }

    @Override
    public A href(Class<? extends Controller> page) {
        return super.href(page);
    }

    @Override
    public Url hrefB(Controller page) {
        return super.hrefB(page);
    }

    public A download(String fileName) {
        return attrs.set("download", fileName).tag;
    }

    /**
     * Specifies what media/device the linked document is optimized for
     *
     * @param mediaQuery
     * @return
     */
    public A media(String mediaQuery) {
        return attrs.set("media", mediaQuery).tag;
    }

    /**
     * Specifies the relationship between the current document and the linked
     * document
     *
     * @param rel
     * @return
     */
    public A rel(LinkType rel) {
        return attrs.setEnum("rel", rel).tag;
    }

    public A target(TargetType targetType) {
        return attrs.setEnum("target", targetType).tag;
    }

    @Override
    public A target(String framename) {
        return super.target(framename);
    }

    @Deprecated
    @Override
    public A a() {
        throw new UnsupportedOperationException();
    }

    @Deprecated
    @Override
    public A a(Object innerText) {
        throw new UnsupportedOperationException();
    }

    public A hrefVoid() {
        return attrs.set("href", "javascript:void(0)").tag;
    }

}

package com.html.core.tag.image;

import com.html.core.dict.Crossorigin;
import com.html.core.tag.Element;
import com.html.core.tag.intfs.*;
import com.utils.Url;

public class Img extends Element<Img> implements Visual<Img>, Closed {

    public Img(Parent parent) {
        super(parent, "img");
    }

    /**
     * Specifies an alternate text for an image
     *
     * @param alt
     * @return
     */
    @Override
    public Img alt(String alt) {
        return super.alt(alt);
    }

    @Override
    public Img height(Number height) {
        return super.height(height);
    }

    @Override
    public Img width(Number width) {
        return super.width(width);
    }

    //Ten atrybut powinien byc dostępny tylko wtedy, jeśli tag img
    //zawiera się w tagu z prawidowym atrybutem href.
    public Img isMap(boolean isMap) {
        return attrs.setState("ismap", isMap).tag;
    }

    @Override
    public Img src(Url src) {
        return super.src(src);
    }

    @Override
    public Url srcB(String src) {
        return super.srcB(src);
    }

    /**
     * Specifies a URL to a detailed description of an image
     *
     * @param longdesc
     * @return
     */
    public Img longdesc(Url longdesc) {
        return attrs.setHref("longdesc", longdesc).tag;
    }

    /**
     * Specifies how the element handles cross-origin requests
     *
     * @param crossorigin
     * @return
     */
    public Img crossorigin(Crossorigin crossorigin) {
        return attrs.setEnum("crossorigin", crossorigin).tag;
    }

}

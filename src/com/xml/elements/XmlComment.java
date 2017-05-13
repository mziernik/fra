package com.xml.elements;

import com.xml.*;

public final class XmlComment extends XmlElement {

    private String innerText;

    public XmlComment(XmlNode parent, String innerText) {
        super(parent, "#Text");
        this.innerText = innerText;
    }

    public String getText() {
        return innerText;
    }

    public XmlComment setText(String text) {
        this.innerText = innerText;
        return this;
    }
}

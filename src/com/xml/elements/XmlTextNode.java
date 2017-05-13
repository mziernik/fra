package com.xml.elements;

import com.xml.*;

public final class XmlTextNode extends XmlElement {

    private String innerText;

    public XmlTextNode(XmlNode parent, String innerText) throws XmlException {
        super(parent, "#Text");
        this.innerText = innerText;
    }

    public String getText() {
        return innerText;
    }

    public XmlTextNode setText(String innerText) {
        this.innerText = innerText;
        return this;
    }
}

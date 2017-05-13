package com.xml.elements;

import com.xml.*;

public final class XmlCdata extends XmlElement {

    public String innerText;

    public XmlCdata(XmlNode parent, String innerText) throws XmlException {
        super(parent, "#Text");
        this.innerText = innerText;
    }
}

package com.xml;

import com.xml.elements.XmlTextNode;
import java.util.HashMap;

public class XmlElement {

    public final HashMap<String, Object> extra = new HashMap<>();
    String name;
    XmlNode parent;

    public Boolean shortTag = null; // null - auto, 1 - wymus krotki, 0 - wymus dlugi

    public int index() {
        if (parent == null)
            return -1;
        return parent.elements.indexOf(this);
    }

    public boolean isTextNode() {
        return (this instanceof XmlTextNode);
    }

    public XmlElement getNext() {
        if (parent == null)
            return null;
        int idx = parent.elements.indexOf(this);
        if (idx == parent.elements.size() - 1)
            return null;
        return parent.elements.get(++idx);
    }

    public XmlElement(XmlNode parent, String name) {
        this.name = name;
        this.parent = parent;
        if (parent != null
                && !(this instanceof XmlNode.XmlDummyNode)
                && !(parent instanceof XmlNode.XmlDummyNode))
            parent.elements.add(this);
    }

    public void delete() {
        if (parent == null)
            return;
        parent.elements.remove(this);
    }
}

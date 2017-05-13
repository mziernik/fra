package com.xml;

public abstract class XmlVisitor {

    public abstract boolean visit(XmlNode node, int level) throws XmlException;
}

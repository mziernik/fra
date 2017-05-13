package com.xml;

public final class XmlAttribute {

    String name;
    String value;
    XmlNode parent;

    @Override
    public String toString() {
        return name + " = " + value;
    }

    public String getName() {
        return name;
    }

    public String getValue() {
        return value;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public XML getXml() {
        return parent != null ? parent.getXML() : null;
    }

    public XmlAttribute(XmlNode parent, int index, String name, String value) throws XmlException {
        XML.checkName(name);
        this.name = name;
        this.value = value;
        this.parent = parent;
        if (parent != null)
            if (index >= 0)
                parent.attributes.add(index, this);
            else
                parent.attributes.add(this);
    }

    public String getPath() {
        return parent.getPath() + " / " + name;
    }

    public void remove() {
        delete();
    }

    public void delete() {
        if (parent == null)
            return;

        for (int i = 0; i < parent.attributes.size(); i++)
            if (parent.attributes.get(i) == this) {
                parent.attributes.remove(i);
                break;
            }
    }
}

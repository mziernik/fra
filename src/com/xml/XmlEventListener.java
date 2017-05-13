package com.xml;

import com.xml.elements.XmlTextNode;
import java.io.Writer;
import java.util.HashMap;

public class XmlEventListener {

    public HashMap extra = new HashMap();

    public boolean beforeWriteNode(Writer writer, XmlNode nd, int lineNumber) {
        return false;
    }

    public boolean beforeWriteTextNode(Writer writer, XmlTextNode node, int lineNumber) {
        return false;
    }

    public boolean beforeWriteAttribute(Writer writer, XmlAttribute attribute, int lineNumber) {
        return false;
    }

    public void afterWriteNode(Writer writer, XmlNode nd, int lineNumber) {
    }

    public boolean onCustomEscapeWrite(Writer writer, XmlNode nd, String s, int lineNumber) {
        // tu mozna podstawic wlasne escapowanie
        return false;
    }
}

package com.xml;

import com.xml.elements.XmlComment;
import com.xml.elements.XmlTextNode;
import java.io.IOException;
import java.io.Writer;

import static com.xml.XML.escape;

public class XmlBuilder {

    private final Writer writer;
    private final XML xml;
    private final Options options;
    int lineNumber = 1;

    public XmlBuilder(XML xml, Writer writer) {
        this.writer = writer;
        this.xml = xml;
        this.options = xml.options;
    }

    public void build(XmlNode node, String space, boolean includeHeder) throws IOException {
        lineNumber = 1;
        if (includeHeder && options.header != null && !options.header.isEmpty()) {
            writer.write(options.header);
            writer.write(options.returnChar);
            ++lineNumber;
        }

        // ------------- zapisz komentarze w glownej galzezi
        if (node == xml)
            for (XmlComment nd : xml.rootComments) {
                write("<!--");
                if (options.trimInnerText)
                    writeEscape(nd.getText().trim(), node, true);
                else
                    writeEscape(nd.getText(), node, true);
                write("-->");
                writeBreakLine(space);
            }

        try {
            enumWrite(node, space);
        } finally {
            writer.flush();
        }
    }

    private void enumWrite(XmlNode node, final String space) throws IOException {
        //    boolean hasAttributes = !node.attributes.isEmpty();
        boolean hasElementNodes = false;
        boolean hasTextNodes = false;
        boolean shortTag = options.useShortTags;
        if (node.shortTag != null)
            shortTag = node.shortTag;

        for (XmlEventListener listener : xml.listeners)
            if (listener.beforeWriteNode(writer, node, lineNumber))
                return;

        for (XmlElement bn : node.elements) {
            if (bn instanceof XmlNode)
                hasElementNodes = true;
            if (bn instanceof XmlTextNode)

                hasTextNodes = true;
            if (hasElementNodes && hasTextNodes)
                break;
        }

        boolean hasNodes = hasElementNodes || hasTextNodes;

        if (node.parent != null)
            writeBreakLine(space);

        write("<" + node.name);

        for (int i = 0; i < node.attributes.size(); i++) {

            XmlAttribute xa = node.attributes.get(i);

            for (XmlEventListener listener : xml.listeners)
                if (listener.beforeWriteAttribute(writer, xa, lineNumber))
                    return;

            if (node.multiLineAttributes) {
                writeBreakLine(space);
                write(options.spaceChar);
            }

            if (!node.multiLineAttributes)
                write(" ");

            write(xa.name + "=\"");
            if (xa.value != null)
                writeEscape(xa.value, node, false);
            write("\"");
        }

        if (!hasNodes && shortTag) {
            if (node.multiLineAttributes)
                writeBreakLine(space);
            write("/>");
        } else {
            write(">");
            if (node.multiLineAttributes && hasTextNodes) {
                writeBreakLine(space);
                write(options.spaceChar);
            }
        }

        for (XmlElement bn : node.elements) {
            if (bn instanceof XmlNode)
                enumWrite((XmlNode) bn, space + options.spaceChar);

            if (bn instanceof XmlComment) {
                XmlComment xtn = (XmlComment) bn;
                writeBreakLine(space);
                write(options.spaceChar);
                write("<!--");

                // komentarze bez escapowania
                if (options.trimInnerText)
                    write(xtn.getText().trim());
                else
                    write(xtn.getText());
                write("-->");
            }

            if (bn instanceof XmlTextNode) {
                XmlTextNode xtn = (XmlTextNode) bn;
                if (xtn.getText() == null)
                    continue;

                for (XmlEventListener listener : xml.listeners)
                    if (listener.beforeWriteTextNode(writer, xtn, lineNumber))
                        return;

                if (options.trimInnerText)
                    writeEscape(xtn.getText().trim(), node, true);
                else
                    writeEscape(xtn.getText(), node, true);
            }
        }

        if (hasNodes)
            if (hasElementNodes)
                writeBreakLine(space);

        if (hasNodes || !shortTag)
            write("</" + node.name + ">");

        for (XmlEventListener listener : xml.listeners)
            listener.afterWriteNode(writer, node, lineNumber);

    }

    private void writeBreakLine(String space) throws IOException {
        if (options.singleLine)
            return;
        write(options.returnChar);
        ++lineNumber;
        write(space);
    }

    private void writeEscape(String s, XmlNode nd, boolean isNode) throws IOException {

        for (XmlEventListener listener : xml.listeners)
            if (listener.onCustomEscapeWrite(writer, nd, s, lineNumber))
                return;
        escape(writer, s, options.escapeWhiteChars || options.singleLine, isNode);
    }

    private void write(String s) throws IOException {
        writer.write(s);
    }
}

package com.xml;

import com.exceptions.ThrowableException;
import com.lang.LXml;
import com.mlogger.Log;
import com.xml.elements.*;
import java.io.*;
import java.net.URL;
import java.util.*;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.*;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author Miłosz Ziernik
 */
public class XML extends XmlNode {

    public final Options options = new Options();

    final List<XmlEventListener> listeners = new LinkedList<>();
    final List<XmlComment> rootComments = new LinkedList<>();

    public DocumentBuilder builder()
            throws ParserConfigurationException, SAXException, IOException {

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        dbf.setValidating(options.validateDTD);
        dbf.setNamespaceAware(false);
        if (!options.validateDTD) {
            dbf.setFeature("http://xml.org/sax/features/namespaces", false);
            dbf.setFeature("http://xml.org/sax/features/validation", false);
            dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
            dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            dbf.setFeature("http://apache.org/xml/features/validation/schema", false);
            dbf.setFeature("http://xml.org/sax/features/external-general-entities", false);

            // dbf.setFeature("http://apache.org/xml/features/validation/identity-constraint-checking", false);
            if (options.entityResolver != null)
                db.setEntityResolver(options.entityResolver);
            else
                db.setEntityResolver(new EntityResolver() {
                    @Override
                    public InputSource resolveEntity(String publicId, String systemId)
                            throws SAXException, IOException {
                        options.ignoredDTD.add(publicId + " - " + systemId);
                        return new InputSource(new StringReader(""));
                    }
                });

        }
        return db;
    }

    static void checkName(String s) throws XmlException {
        if (s == null || s.trim().isEmpty()
                || !(s.charAt(0) == '_'
                || (s.charAt(0) >= 'a' && s.charAt(0) <= 'z')
                || (s.charAt(0) >= 'A' && s.charAt(0) <= 'Z')))
            throw new XmlException(LXml.INVALID_NAME.toString(s));

    }

    public XML(String sxml) throws XmlException {
        super();
        load(sxml);
    }

    public void load(String sxml) throws XmlException {
        try {
            InputSource is = new InputSource();
            is.setCharacterStream(new StringReader(sxml));
            parse(builder().parse(is));
        } catch (Exception ex) {
            throw new XmlException(ex);
        }
    }

    public XML(File file) throws XmlException {
        super();
        load(file);
    }

    public XML(Document document) {
        super();
        try {
            parse(document);
        } catch (XmlException ex) {
            throw new ThrowableException(ex);
        }
    }

    public XML() {
        super();
        try {
            load("<xml/>");
        } catch (XmlException xe) {
            Log.error(xe);
        }
    }

    public void load(File file) throws XmlException {
        try {
            parse(builder().parse(file));
        } catch (Exception ex) {
            throw new XmlException(ex);
        }
    }

    public XML(byte[] bxml) throws XmlException {
        super();
        load(bxml);
    }

    public void load(byte[] bxml) throws XmlException {
        try {
            parse(builder().parse(new ByteArrayInputStream(bxml)));
        } catch (Exception ex) {
            throw new XmlException(ex);
        }
    }

    public XML(InputStream is) throws XmlException {
        super();
        load(is);
    }

    public XML(URL url) throws XmlException {
        super();
        load(url);
    }

    public void load(URL url) throws XmlException {
        try {
            parse(builder().parse(url.toString()));
        } catch (Exception ex) {
            throw new XmlException(ex);
        }
    }

    public void load(InputStream is) throws XmlException {
        try {
            parse(builder().parse(is));
        } catch (Exception ex) {
            throw new XmlException(ex);
        }
    }

    private void enumNode(Node node, XmlNode xnode) throws XmlException {

        NamedNodeMap attribs = node.getAttributes();

        for (int i = 0; i < attribs.getLength(); i++) {
            Node nd = attribs.item(i);
            new XmlAttribute(xnode, -1, nd.getNodeName(), nd.getNodeValue());
        }

        NodeList nList = node.getChildNodes();

        for (int n = 0; n < nList.getLength(); n++) {
            Node nd = nList.item(n);

            switch (nd.getNodeType()) {
                case Element.ELEMENT_NODE:
                    enumNode(nd, new XmlNode(xnode, nd.getNodeName()));
                    break;

                case Element.TEXT_NODE:
                    if (!nd.getNodeValue().trim().isEmpty())
                        new XmlTextNode(xnode, nd.getNodeValue());
                    break;

                case Element.COMMENT_NODE:
                    new XmlComment(xnode, nd.getNodeValue());
                    break;

                case Element.CDATA_SECTION_NODE:
                    new XmlCdata(xnode, nd.getNodeValue());
                    break;

            }
        }
    }

    private void parse(Document document) throws XmlException {
        Element eroot = document.getDocumentElement();

        NodeList nList = document.getChildNodes();

        for (int n = 0; n < nList.getLength(); n++) {
            Node nd = nList.item(n);
            if (nd.getNodeType() == Element.COMMENT_NODE)
                rootComments.add(new XmlComment(null, nd.getNodeValue()));
        }
        name = eroot.getNodeName();
        enumNode(document.getDocumentElement(), this);
    }

    public static void escape(Writer writer, String s, boolean escapeWhiteChars, boolean isNode) throws IOException {
        if (writer == null || s == null)
            return;

        int len = s.length();

        //  znaki zastrzeżone w XMLu 	[#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]
        for (int i = 0; i < len; i++) {
            char c = s.charAt(i);

            if ((c >= 0x0 && c <= 0x8)
                    || c == 0xB
                    || c == 0xC
                    || c == 0xE
                    || (c >= 0xF && c <= 0x1F)
                    || (c >= 0x7F && c <= 0x84)
                    || (c >= 0x86 && c <= 0x97))
                continue;

            if (escapeWhiteChars && c < 32) {
                writer.append("&#")
                        .append(Integer.toString(c))
                        .append(";");
                continue;
            }
            if (isNode)
                switch (c) {
                    case '<':
                        writer.write("&lt;");
                        break;
                    case '>':
                        writer.write("&gt;");
                        break;
                    case '&':
                        writer.write("&amp;");
                        break;
                    default:
                        writer.write(c);
                        break;
                }
            if (!isNode)
                switch (c) {
                    case '&':
                        writer.write("&amp;");
                        break;
                    case '"':
                        writer.write("&quot;");
                        break;
//                case '\'':
//                    writer.write("&apos;");
//                    break;
                    default:
                        writer.write(c);
                        break;
                }
        }
    }

    public static String escape(String s, boolean isNode) {
        StringWriter writer = new StringWriter();
        try {
            escape(writer, s, false, isNode);
        } catch (IOException ex) {
        }
        return writer.toString();
    }

    public void save(File file) throws IOException {
        new XmlBuilder(this, new BufferedWriter(new FileWriter(file), 102400)).build(this, "", true);
    }

    public XML addListener(XmlEventListener listener) {
        listeners.add(listener);
        return this;
    }

}

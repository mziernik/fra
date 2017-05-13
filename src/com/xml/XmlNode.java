package com.xml;

import com.exceptions.ThrowableException;
import com.utils.vparser.VParser;
import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.json.JCollection;
import com.lang.LXml;
import com.xml.XmlSerializer.INode;
import com.mlogger.Log;
import com.utils.*;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.xml.elements.XmlComment;
import com.xml.elements.XmlTextNode;
import java.io.*;
import java.util.*;

public class XmlNode extends XmlElement implements Iterable<XmlNode> {

    final List<XmlElement> elements = new LinkedList<>();
    final List<XmlAttribute> attributes = new LinkedList<>();
    public boolean multiLineAttributes = false;
    public INode annotation;

    public String toString(boolean includeHeader) {
        try {
            StringWriter sw = new StringWriter();
            new XmlBuilder(getXML(), sw).build(this, "", includeHeader);
            return sw.toString();
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
    }

    @Override
    public String toString() {
        return toString(false);
    }

    public byte[] toByteArray() throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        OutputStreamWriter ow = new OutputStreamWriter(bout, Utils.UTF8);
        try {
            new XmlBuilder(getXML(), ow).build(this, "", true);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
        return bout.toByteArray();
    }

    public XmlNode getParent() throws IOException {
        return parent;
    }

    public XML getXML() {
        XmlNode node = this;
        while (node != null) {
            if (node instanceof XML)
                return (XML) node;
            node = node.parent;
        }
        return null;

    }

    public HashMap<String, String> getDeclaredNamespaces() {
        HashMap<String, String> ns = new HashMap<>();
        for (XmlAttribute a : attributes)
            if (a.name != null && a.name.toLowerCase().startsWith("xmlns:"))
                ns.put(a.value != null ? a.value : null,
                        a.name.substring(a.name.indexOf(":") + 1));
        return ns;
    }

    private boolean compare(String name, String mask, boolean useMask) {

        if (useMask && mask != null && name != null)
            return Str.matchesMask(name, mask);

        return mask != null && name != null && (getXML().options.caseSensitive
                ? mask.equals(name)
                : mask.equalsIgnoreCase(name));
    }

    // konstruktor dla klasy Xml
    XmlNode() {
        super(null, null);
    }

    public XmlNode(XmlNode parent, String name) throws XmlException {
        super(parent, name);
        if (!(this instanceof XmlDummyNode))
            XML.checkName(name);
    }

    public String getNamespace() {
        if (name.indexOf(":") > 0)
            return name.substring(0, name.indexOf(":"));
        return "";
    }

    public XmlNode clearAll() {
        elements.clear();
        attributes.clear();
        return this;
    }

    public XmlNode clearNodes() {
        elements.clear();
        return this;
    }

    public XmlNode clearAttributes() {
        attributes.clear();
        return this;
    }

    public void deleteAtribute(String name) throws XmlException {
        XmlAttribute attr = getAttribute(name);
        if (attr != null)
            attr.delete();
    }

    public boolean has(String nodeName) {
        for (XmlNode node : getNodes())
            if (compare(node.getName(), nodeName, false))
                return true;
        return false;
    }

    public Strings getPath() {
        return getPath(null);
    }

    public Strings getPath(String extraPath) {
        Strings result = new Strings();
        result.separator("/");
        XmlNode nd = this;
        while (nd != null) {
            result.insert(nd.name);
            nd = nd.parent;
        }

        if (extraPath != null)
            result.add(extraPath.split("/"));

        return result;
    }

    private void enumGetNodes(List<XmlNode> list, XmlNode node,
            String[] path, int level, boolean canCreate) throws XmlException {

        if (path == null || level >= path.length)
            return;

        boolean found = false;
        for (XmlElement xn : node.elements) {

            if (!(xn instanceof XmlNode))
                continue;

            XmlNode nd = (XmlNode) xn;

            if (compare(xn.name, path[level], true)) {
                if (level >= path.length - 1) {
                    list.add(nd);
                    found = true;
                    continue;
                }
                enumGetNodes(list, nd, path, level + 1, canCreate);
            }
        }

        // jesli nie znaleziono galezi na podstawie samej nazwy, sprobuj
        // poszukac dodajac przestrzen nazw rodzica
        if (!found && getXML().options.useNameSpaces && !node.getNamespace().isEmpty()) {
            String ns = node.getNamespace();

            for (XmlElement xn : node.elements) {

                if (!(xn instanceof XmlNode))
                    continue;

                XmlNode nd = (XmlNode) xn;

                if (compare(xn.name, ns + ":" + path[level], true)) {
                    if (level >= path.length - 1) {
                        list.add(nd);
                        continue;
                    }
                    found = true;
                    enumGetNodes(list, nd, path, level + 1, canCreate);
                }
            }
        }

        if (!found && canCreate) {
            XmlNode nd = new XmlNode(node, path[level]);
            if (level == path.length - 1)
                list.add(nd);
            enumGetNodes(list, nd, path, level + 1, canCreate);
        }
    }

    private String[] split(String path) {
        return path == null ? new String[0] : path.split("/");
    }

    public XmlNode addNode(String path) throws XmlException {
        String[] split = split(path);
        if (split == null || split.length == 0)
            return this;

        XmlNode nd = this;
        String[] pp = Arrays.copyOf(split, split.length - 1);
        if (pp.length > 0)
            nd = getNode(false, new Strings(pp).toString("/"));
        return new XmlNode(nd, split[split.length - 1]);
    }

    /**
     * Zwraca gałąź o danej ścieżce, jeśli nie istnieje zwróć atrapę
     *
     * @param path
     * @return
     * @throws com.xml.XmlException
     */
    public XmlNode nodeD(String path) throws XmlException {
        XmlNode node = getNode(null, path);
        return node != null ? node : new XmlDummyNode(this, "dummy");
    }

    public XmlNode nodeF(String path) throws XmlException {
        return getNode(true, path);
    }

    /**
     * Zwraca gałąź o danej ścieżce, jeśli nie istnieje to tworzy
     *
     * @param path
     * @return
     * @throws com.xml.XmlException
     */
    public XmlNode nodeC(String path) throws XmlException {
        return getNode(false, path);
    }

    /**
     * Zwraca gałąź o danej ścieżce, jeśli nie istnieje zwraca null-a
     *
     * @param path
     * @return
     */
    public XmlNode node(String path) {
        try {
            return getNode(null, path);
        } catch (XmlException ex) {
            Log.warning(ex);
            return null;
        }
    }

    /**
     * Otwiera gałąź, mustExists: true: jesli nie znaleziono, zwraca bląd false:
     * jesli nie znalezino, to utworz null: jesli nie znaleziono, to zwroc nulla
     */
    private XmlNode getNode(Boolean mustExists, String nodePath)
            throws XmlException {

        List<XmlNode> lNodes = nodes(mustExists, nodePath);

        if (!lNodes.isEmpty())
            return lNodes.get(0);

        if (lNodes.isEmpty() && mustExists == null)
            return null;

        throw new XmlException(LXml.BRANCH_NOT_FOUND.toString(getPath().add(nodePath)));

    }

    public LinkedList<XmlNode> getNodes() {
        LinkedList<XmlNode> list = new LinkedList<>();
        for (XmlElement el : elements)
            if (el instanceof XmlNode)
                list.add((XmlNode) el);
        return list;
    }

    public List<XmlElement> getElements() {
        List<XmlElement> list = new LinkedList<>();
        list.addAll(elements);
        return list;
    }

    public LinkedList<XmlNode> getNodes(String nodePath) {
        try {
            return nodes(null, nodePath);
        } catch (XmlException ex) {
            Log.warning(ex);
            return new LinkedList<>();
        }
    }

    /**
     * Tworzy listę gałęzi, mustExists: true: jesli nie znaleziono, zwraca bląd
     * false: jesli nie znalezino, to utworz null: jesli nie znaleziono, to
     * zwroc nulla
     */
    private LinkedList<XmlNode> nodes(Boolean mustExists, String nodePath)
            throws XmlException {

        String[] split = split(nodePath);
        // jesli nie podano sciezki to zwróć bieżąca
        if (nodePath == null || split.length == 0) {
            LinkedList<XmlNode> list = new LinkedList<>();
            for (XmlElement bn : elements)
                if (bn instanceof XmlNode)
                    list.add((XmlNode) bn);
            return list;
        }

        LinkedList<XmlNode> list = new LinkedList<>();

        enumGetNodes(list, this, split, 0, mustExists != null && !mustExists);
        if (list.isEmpty())
            if (mustExists != null && mustExists)
                throw new XmlException(LXml.BRANCH_NOT_FOUND.toString(
                        getPath().add(nodePath)));

        return list;
    }

    public boolean hasText() {
        for (XmlElement el : elements)
            if (el instanceof XmlTextNode)
                return true;
        return false;
    }

    public List<String> getInnerTexts() {
        List<String> result = new LinkedList<>();
        for (XmlElement el : elements)
            if (el instanceof XmlTextNode)
                result.add(((XmlTextNode) el).getText());

        return result;
    }

    public void deleteTextNodes() {
        for (int i = elements.size() - 1; i >= 0; i--) {
            XmlElement bn = elements.get(i);
            if (bn instanceof XmlElement)
                bn.delete();
        }
    }

    public XmlTextNode addText(Object text) throws XmlException {
        if (text == null)
            return null;
        return new XmlTextNode(this, Utils.toString(text));
    }

    public XmlNode addText(String nodePath, Object text) throws XmlException {
        if (text == null)
            return null;
        addNode(nodePath).addText(text);
        return this;
    }

    public XmlComment addComment(String text) throws XmlException {
        return new XmlComment(this, text);
    }

    public XmlTextNode setText(Object text) throws XmlException {
        deleteTextNodes();
        return addText(Utils.toString(text));
    }

    public XmlNode setText(String nodePath, Object text) throws XmlException {
        nodeC(nodePath).setText(Utils.toString(text));
        return this;
    }

    /**
     * Zwraca listę wartości tekstowych z gałęzi nodepath
     *
     * @param nodePath
     * @param includeNull
     * @return
     */
    public LinkedList<String> getTextValues(String nodePath, boolean includeNull) {
        LinkedList<String> lst = new LinkedList<>();
        for (XmlNode node : getNodes(nodePath)) {
            String txt = node.getText();
            if (txt != null || includeNull)
                lst.add(txt);
        }
        return lst;
    }

    public String getTextF(String nodePath) throws XmlException {
        return getNode(true, nodePath).getText();
    }

    /* public String getText(String nodePath) {
     try {
     XmlNode node = getNode(false, nodePath);
     return node != null ? node.getText() : null;
     } catch (XmlException ex) {
     return null;
     }
     }
     */
    public String getText() {
        List<String> ss = getInnerTexts();
        StrWriter sb = new StrWriter();
        for (String s : ss)
            sb.append(s);
        return ss.isEmpty() ? "" : sb.toString();
    }

    public List<XmlTextNode> getTextNodes() throws XmlException {
        List<XmlTextNode> result = new LinkedList<>();
        for (XmlElement nd : elements)
            if (nd instanceof XmlTextNode)
                result.add((XmlTextNode) nd);
        return result;
    }

    public XmlAttribute getAttribute(String path) throws XmlException {
        return getAttribute(path, false);
    }

    public XmlAttribute getAttribute(String path, boolean mustExists)
            throws XmlException {
        List<XmlAttribute> lAttribs = getAttributes(mustExists, path);

        if (!lAttribs.isEmpty())
            return lAttribs.get(0);

        if (!mustExists)
            return null;

        throw new XmlException(LXml.ATTRIBUTE_NOT_FOUND_ARG.toString(
                getPath().add(path)));

    }

    public LinkedList<XmlAttribute> getAttributesF(String path)
            throws XmlException {
        return getAttributes(true, path);
    }

    public LinkedList<XmlAttribute> getAttributes()
            throws XmlException {
        LinkedList<XmlAttribute> list = new LinkedList<>();
        list.addAll(attributes);
        return list;
    }

    public LinkedList<XmlAttribute> getAttributes(String path)
            throws XmlException {
        return getAttributes(false, path);
    }

    private LinkedList<XmlAttribute> getAttributes(boolean mustExists, String path)
            throws XmlException {

        String[] split = split(path);

        if (split.length == 0) {
            if (mustExists && attributes.isEmpty())
                throw new XmlException(LXml.ATTRIBUTE_NOT_FOUND_ARG.toString());
            return getAttributes();
        }

        String[] fNodes = null;
        String sAttr = split[split.length - 1];

        if (split.length > 1)
            fNodes = Arrays.copyOf(split, split.length - 1);

        LinkedList<XmlAttribute> lAttribs = new LinkedList<>();
        List<XmlNode> lNodes;
        if (fNodes == null || fNodes.length == 0) {
            lNodes = new LinkedList<>();
            lNodes.add(this);
        } else
            lNodes = nodes(true, new Strings(fNodes).toString("/"));

        for (XmlNode nd : lNodes) {

            XmlAttribute attr = null;

            for (XmlAttribute xa : nd.attributes)
                if (compare(xa.name, sAttr, true)) {
                    attr = xa;
                    break;
                }

            if (attr == null && getXML().options.useNameSpaces && !nd.getNamespace().isEmpty()) {
                String ns = nd.getNamespace();
                for (XmlAttribute xa : nd.attributes)
                    if (compare(xa.name, ns + ":" + sAttr, true)) {
                        attr = xa;
                        break;
                    }
            }

            if (attr != null)
                lAttribs.add(attr);

        }

        if (lAttribs.isEmpty())
            if (mustExists)
                throw new XmlException(LXml.ATTRIBUTE_NOT_FOUND_ARG.toString(
                        getPath(sAttr)));
            else
                return lAttribs;

        return lAttribs;
    }

    public XmlNode attr(String name, Object value) throws XmlException {
        if (value == null)
            return this;
        XmlAttribute attr = null;
        for (XmlAttribute a : attributes)
            if (compare(a.name, name, true)) {
                attr = a;
                break;
            }

        if (attr == null)
            attr = new XmlAttribute(this, -1, name, value.toString());

        attr.value = value.toString();

        return this;
    }

    /* private Object getAttr(String... path) {
     try {
     XmlAttribute aa = attribute(false, path);
     return aa != null && aa.value != null ? aa.value : null;
     } catch (Exception e) {
     return null;
     }
     } */
    private VParser parseAttr(String path) {
        XmlAttribute attr = null;
        try {
            attr = getAttribute(path, false);
        } catch (XmlException ex) {
            Log.warning(ex);
        }

        VParser parser = new VParser(getPath(path).toString(" / "),
                attr != null ? attr.value : null,
                VParser.Option.DEF_ON_ERR,
                VParser.Option.LENIENT,
                attr != null ? VParser.Option.EXISTS : null);

        parser.eNullValue = LXml.ATTRIBUTE_NOT_FOUND_ARG_2;
        parser.eEmptyValue = LXml.ATTRIBUTE_VALUE_CANT_BE_EMPTY;
        parser.eMissingValue = LXml.ATTRIBUTE_NOT_FOUND_ARG_2;
        parser.eIncorrectValue = LXml.INVALID_ATTRIBUTE_VALUE;
        parser.eIncorrectType = LXml.INVALID_ATTRIBUTE_VALUE_TYPE;

        return parser;
    }

    public String attrStr(String path) {
        return parseAttr(path).getStr();
    }

    public String attrStr(String path, String def) {
        return parseAttr(path).getStr(def);
    }

    public boolean attrBool(String path) {
        return parseAttr(path).getBool();
    }

    public Boolean attrBool(String path, Boolean def) {
        return parseAttr(path).getBool(def);
    }

    public byte attrByte(String path) {
        return parseAttr(path).getByte();
    }

    public Byte attrByte(String path, Byte def) {
        return parseAttr(path).getByte(def);
    }

    public short attrShort(String path) {
        return parseAttr(path).getShort();
    }

    public Short attrShort(String path, Short def) {
        return parseAttr(path).getShort(def);
    }

    public int attrInt(String path) {
        return parseAttr(path).getInt();
    }

    public Integer attrInt(String path, Integer def) {
        return parseAttr(path).getInt(def);
    }

    public long attrLong(String path) {
        return parseAttr(path).getLong();
    }

    public Long attrLong(String path, Long def) {
        return parseAttr(path).getLong(def);
    }

    public float attrFloat(String path) {
        return parseAttr(path).getFloat();
    }

    public Float attrFloat(String path, Float def) {
        return parseAttr(path).getFloat(def);
    }

    public double attrDouble(String path) {
        return parseAttr(path).getDouble();
    }

    public Double attrDouble(String path, Double def) {
        return parseAttr(path).getDouble(def);
    }

    public TCurrency attrCurrency(String path) {
        return parseAttr(path).getCurrency();
    }

    public TCurrency attrCurrency(String path, TCurrency def) {
        return parseAttr(path).getCurrency(def);
    }

    public Date attrDate(String path) {
        return parseAttr(path).getDate();
    }

    public TDate attrDate(String path, TDate def) {
        return parseAttr(path).getDate(def);
    }

    private VParser getVal(String path) {
        XmlNode node = null;
        try {
            node = getNode(null, path);
        } catch (XmlException ex) {
        }

        return new VParser(getPath(path).toString(" / "),
                node != null ? node.getText() : null,
                VParser.Option.LENIENT,
                VParser.Option.DEF_ON_ERR,
                node != null ? VParser.Option.EXISTS : null);
    }

    public String getStr(String path) {
        return getVal(path).getStr();
    }

    public String getStr(String path, String def) {
        return getVal(path).getStr(def);
    }

    public Boolean getBool(String path, Boolean def) {
        return getVal(path).getBool(def);
    }

    public boolean getBool(String path) {
        return getVal(path).getBool();
    }

    public short getShort(String path) {
        return getVal(path).getShort();
    }

    public Short getShort(String path, Short def) {
        return getVal(path).getShort(def);
    }

    public int getInt(String path) {
        return getVal(path).getInt();
    }

    public Integer getInt(String path, Integer def) {
        return getVal(path).getInt(def);
    }

    public long getLong(String path) {
        return getVal(path).getLong();
    }

    public Long getLong(String path, Long def) {
        return getVal(path).getLong(def);
    }

    public float getFloat(String path) {
        return getVal(path).getFloat();
    }

    public Float getFloat(String path, Float def) {
        return getVal(path).getFloat(def);
    }

    public double getDouble(String path) {
        return getVal(path).getDouble();
    }

    public TCurrency getCurrency(String path) {
        return getVal(path).getCurrency();
    }

    public TCurrency getCurrency(String path, TCurrency def) {
        return getVal(path).getCurrency(def);
    }

    public Double getDouble(String path, Double def) {
        return getVal(path).getDouble(def);
    }

    public TDate getDate(String path) {
        return getVal(path).getDate();
    }

    public TDate getDate(String path, TDate def) {
        return getVal(path).getDate(def);
    }

    /*
     public XmlNode addNode(String... nodePath) throws XmlException {
     String[] nn = nodePath.split(xml.pathSeparator);

     XmlNode base = this;

     for (String s : nn) {
     if (!s.trim().isEmpty()) {
     base = new XmlNode(base, s);
     }
     }
     return base;
     }
     */
    public void move(XmlNode newParent, XmlElement destination, boolean insertAfter) {
        if (newParent == null || destination == null || destination.parent == null)
            return;

        int idx = destination.parent.elements.indexOf(destination);

        if (idx == -1 || idx == elements.size() - 1)
            newParent.elements.add(this);
        else
            newParent.elements.add(insertAfter ? idx + 1 : idx, this);
        if (parent != null)
            parent.elements.remove(this);
        this.parent = newParent;
    }

    public void move(XmlNode newParent) {
        if (newParent == null)
            return;
        newParent.elements.add(this);
        if (parent != null)
            parent.elements.remove(this);
        parent = newParent;
    }

    /**
     * Znajdź gałąź, jeśli nie istnieje to utwórz
     */
    private void enumFind(List<XmlNode> lst, XmlNode node, String name) {

        for (int i = 0; i < node.elements.size(); i++) {
            if (!(node.elements.get(i) instanceof XmlNode))
                continue;
            XmlNode nn = (XmlNode) node.elements.get(i);
            if (compare(nn.name, name, true))
                lst.add(nn);
            enumFind(lst, nn, name);
        }
    }

    /**
     * Znajdz galaz na podtsawie nazyw przeszukujac drzewo
     */
    public List<XmlNode> findNodes(String name) {
        List<XmlNode> lst = new LinkedList<>();
        enumFind(lst, this, name);
        return lst;
    }

    public void visit(XmlVisitor proc) throws XmlException {
        if (proc != null)
            visit(this, 0, proc);
    }

    private boolean visit(XmlNode node, int level, XmlVisitor proc) throws XmlException {

        if (!proc.visit(node, level))
            return false;

        for (XmlNode nd : node.getNodes()) {
            if (!proc.visit(nd, level + 1))
                return false;
            if (!visit(nd, level + 1, proc))
                return false;
        }
        return true;
    }

    public void write(Writer writer) throws IOException {
        new XmlBuilder(getXML(), writer).build(this, "", true);
    }

    public void write(Writer writer, String space) throws IOException {
        new XmlBuilder(getXML(), writer).build(this, space, true);
    }

    public String getName() {
        return name;
    }

    public String getName(boolean includeNameSpace) {
        String name = this.name;
        if (!includeNameSpace && name.contains(":"))
            return name.substring(name.indexOf(":") + 1);
        return name;
    }

    public XmlNode setName(String name) {
        this.name = name;
        return this;
    }

    public JCollection toJson(final JCollection json) throws XmlException {
        return toJson(json, true, true);
    }

    public JCollection toJson(final JCollection json, boolean valuesOnly) throws XmlException {
        return toJson(json, valuesOnly, true);
    }

    public JCollection toJson(final JCollection json, boolean valuesOnly,
            final boolean includeNameSpaces) throws XmlException {

        new Object() {
            void visit(XmlNode node, JCollection col) throws XmlException {

                col.getRoot();

                List<XmlNode> nodes = node.getNodes();

                boolean valuesOnly = false;
                for (XmlNode nd : nodes)
                    valuesOnly = nd.getNodes().isEmpty();

                if (valuesOnly && col.isArray()) {
                    visit(node, col.asArray().object());
                    return;
                }

                for (XmlNode nd : nodes) {

                    boolean isArray = false;
                    boolean isValue = nd.getNodes().isEmpty();

                    for (XmlNode nd2 : nodes)
                        isArray |= nd != nd2 && nd.getName(includeNameSpaces).equals(nd2.getName(includeNameSpaces));

                    JCollection el = isArray
                            ? col.arrayC(nd.getName(includeNameSpaces))
                            : isValue ? col
                                    : col.objectC(nd.getName(includeNameSpaces));

                    if (isValue)
                        el.value(nd.getName(includeNameSpaces), nd.getText());
                    else
                        visit(nd, el);
                }

            }
        }.visit(this, json.objectC(getName()));
        return json;
    }

    public boolean isEmpty() {
        return elements.isEmpty();
    }

    public void add(int idx, XmlElement node) {
        elements.add(idx, node);
        node.parent = this;
    }

    public void add(XmlElement node) {
        elements.add(node);
        node.parent = this;
    }

    @Override
    public Iterator<XmlNode> iterator() {
        return getNodes().iterator();
    }

    public static class XmlDummyNode extends XmlNode {

        public XmlDummyNode(XmlNode parent, String name) throws XmlException {
            super(parent, name);
        }
    }
}

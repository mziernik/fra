package com.utils;

import com.utils.text.StrWriter;
import com.utils.collections.Strings;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;

/**
 * Created on : 2012-06-04, 09:41:23 Author : Miłosz Ziernik
 */
public class Path {

    //ToDo: Obsługa ścieżek zasobów, np //10.0.0.1/www
    //ToDo: Dodać obsługę slasha na końcu ściezki np : /home/
    public class URIPath {

        public String protocol = null;
        public String username = null;
        public String password = null;
        public String host = null;
        public int port = 0;
        public final Strings path = new Strings().separator("/");
        public String query = null;

        @Override
        public String toString() {

            StrWriter sb = new StrWriter();

            sb.append(protocol != null && !protocol.isEmpty() ? protocol
                    : "file")
                    .append("://");

            String sPath = Path.this.getPath();

            if (host != null
                    && !host.isEmpty()
                    && username != null
                    && !username.isEmpty()) {

                sb.append(username);

                if (password != null && !password.isEmpty())
                    sb.append(":").append(password);

                sb.append("@");
                sb.append(host);
                if (port > 0)
                    sb.append(":").append(Integer.toString(port));

                if (!sPath.isEmpty())
                    sb.append("/");
            }

            if (!sPath.isEmpty())
                sb.append(sPath);

            if (query != null && !query.isEmpty())
                sb.append("?").append(query);

            return sb.toString();
        }

        @Override
        public int hashCode() {
            int hash = 7;
            hash = 31 * hash + Objects.hashCode(this.protocol);
            hash = 31 * hash + Objects.hashCode(this.username);
            hash = 31 * hash + Objects.hashCode(this.password);
            hash = 31 * hash + Objects.hashCode(this.host);
            hash = 31 * hash + this.port;
            hash = 31 * hash + Objects.hashCode(this.path);
            hash = 31 * hash + Objects.hashCode(this.query);
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            final URIPath other = (URIPath) obj;
            if (!Objects.equals(this.protocol, other.protocol))
                return false;
            if (!Objects.equals(this.username, other.username))
                return false;
            if (!Objects.equals(this.password, other.password))
                return false;
            if (!Objects.equals(this.host, other.host))
                return false;
            if (this.port != other.port)
                return false;
            if (!Objects.equals(this.path, other.path))
                return false;
            if (!Objects.equals(this.query, other.query))
                return false;
            return true;
        }

        private boolean build(Strings elements) {
            boolean isNonFileUri = false;
            host = null;
            path.clear();
            path.addAll(elements);

            if (protocol != null
                    && !protocol.isEmpty()
                    && !protocol.equalsIgnoreCase("file")) {

                host = path.first(true);
                isNonFileUri = true;

                if (host.contains(":")) {
                    port = Utils.strInt(host.substring(host.lastIndexOf(":") + 1), 0);
                    host = host.substring(0, host.lastIndexOf(":"));
                }
            }
            return isNonFileUri;
        }
    }

    private final Strings elements = new Strings().separator("/");
    //public String separator = "/";
    private boolean absolute = false; // czy sciezka zaczyna sie ok X:\ lub /
    private boolean windowsAbsolute = false; // drugi znak sciezki to ':'
    private boolean readOnly = false;
    private boolean processEnvironments = true;
    //ToDo: Dodać obsługę zmiennych
    private boolean processVariables = true;
    private Boolean forceAbsolute = null;
    public boolean caseSensitive = true;
    public final URIPath uri = new URIPath();
    private Path root;
    private BasicFileAttributes attributes;

    private final Set<IPathChange> listenners = new LinkedHashSet<>();

    public static interface IPathChange {

        public boolean onPathChange(Strings elements, boolean added);
    }

    public BasicFileAttributes getAttributes() throws IOException {
        if (attributes == null)
            attributes = Files.readAttributes(toFile().toPath(), BasicFileAttributes.class);

        return attributes;
    }

    public Path setAttributes(BasicFileAttributes attributes) {
        this.attributes = attributes;
        return this;
    }

    public Path addOnChangeListenner(IPathChange listenner) {
        listenners.add(listenner);
        return this;
    }

    public Path removeOnChangeListenner(IPathChange listenner) {
        listenners.remove(listenner);
        return this;
    }

    private LinkedList<String> buildPath(Object... sPath) {
        LinkedList<String> list = new LinkedList<>();
        if (sPath == null)
            return list;

        boolean first = true;

        for (Object ss : sPath) {
            if (ss == null)
                continue;

            String elm = ss.toString();

            if (elm.contains("?")) {
                uri.query = elm.substring(elm.indexOf("?") + 1, elm.length());
                elm = elm.substring(0, elm.indexOf("?"));
            }

            if (first && elm.contains("://")) {
                uri.protocol = elm.substring(0, elm.indexOf("://"));
                elm = elm.substring(uri.protocol.length() + 3, elm.length());
            }

            if (processEnvironments)
                elm = Variables.processSystemEnvironments(elm,
                        ' ', '<', '>', ':', '"', '/', '|', '?', '*');

            elm = elm.replace("\\", "/");

            if (first && forceAbsolute == null && elements.isEmpty()) {
                String s = elm.replace("/", "").replace(" ", "");

                // jesli zaczyna sie od '/' lub drugim znakiem jest ':'
                absolute = elm.startsWith("/");
                absolute |= windowsAbsolute = s.length() > 1 && s.substring(1, 2).equals(":");
            }

            String[] lst = elm.split("/", -1);
            for (String part : lst) {
                if (part.trim().isEmpty())
                    continue;

                if (first
                        && uri.protocol != null
                        && !uri.protocol.isEmpty()
                        && !uri.protocol.equalsIgnoreCase("file")
                        && part.contains("@")) {
                    String a = part.substring(0, part.indexOf("@"));
                    part = part.substring(a.length() + 1, part.length());

                    String[] elms = a.split(":");
                    uri.username = elms[0];
                    if (elms.length > 1)
                        uri.password = elms[1];
                }

                list.add(part.trim());
                first = false;
            }
            first = false;
        }

        int idx;
        while ((idx = list.indexOf("..")) >= 0) {
            list.remove(idx);
            if (idx > 0)
                list.remove(idx - 1);
        }

        return list;
    }

    public Path absolute(Boolean isAbsolute) {
        forceAbsolute = isAbsolute;
        if (forceAbsolute != null)
            absolute = forceAbsolute;
        return this;
    }

    public Path clear() {
        elements.clear();
        absolute = false;
        windowsAbsolute = false;
        uri.protocol = null;
        uri.host = null;
        uri.password = null;
        uri.path.clear();
        uri.query = null;
        uri.username = null;
        return this;
    }

    public Path set(Object... path) {
        return _add(true, path);
    }

    public Path add(Iterable<String>... path) {
        if (path != null)
            for (Iterable<String> itr : path)
                if (itr != null)
                    for (String s : itr)
                        add(s);
        return this;
    }

    public final Path add(Object... path) {
        return _add(false, path);
    }

    private Path _add(boolean set, Object... path) {
        if (readOnly)
            return this;

        Strings elms = new Strings(path);
        for (IPathChange pc : listenners)
            if (!pc.onPathChange(elms, !set));

        if (set)
            clear();

        LinkedList<String> list = buildPath(path);

        if (list.isEmpty())
            return this;

        elements.addAll(list);
        // jesli jest to adres uri i nie jest to protokol 'file' 
        //to usun pierwszy element (nazwe hosta) ze sciezki
        if (uri.build(elements))
            elements.remove(0);

        return this;
    }

    /**
     * Zwraca ostatni element sciezki
     *
     * @param remove usun element z listy
     * @return
     */
    public String last(boolean remove) {
        if (readOnly)
            remove = false;
        String el = elements.last(remove);
        if (remove)
            uri.build(elements);
        return el;
    }

    /**
     * Zwraca pierwszy element sciezki
     *
     * @param remove usun element z listy
     * @return
     */
    public String first(boolean remove) {
        if (readOnly)
            remove = false;
        String el = elements.first(remove);
        if (remove)
            uri.build(elements);
        return el;
    }

    public Path(Object... path) {
        add(path);
    }

    public Path readOnly(boolean readOnly) {
        this.readOnly = readOnly;
        return this;
    }

    public boolean getReadOnly() {
        return readOnly;
    }

    public boolean processEnvironments() {
        return processEnvironments;
    }

    public Path processEnvironments(boolean processEnvironments) {
        this.processEnvironments = processEnvironments;
        return this;
    }

    public String getFileName() {
        if (elements.isEmpty())
            return "";
        return elements.get(elements.size() - 1);
    }

    public File getFile(Object... path) {
        return new File(getPath(path));
    }

    public File toFile() {
        return new File(getPath());
    }

    public boolean exists() {
        return toFile().exists();
    }

    /**
     * 0 : bieżący
     */
    public Path getParent(int level) {
        Path p = this;
        for (int i = 0; i < level; i++) {
            p = p.getParent();
            if (p == null)
                return null;
        }
        return p;
    }

    public Path getParent() {
        String res = isAbsolute() ? "/" : "";

        for (int i = 0; i < elements.size() - 1; i++)
            res += elements.get(i) + "/";
        return new Path(res);
    }

    public String getPath(Object... path) {
        Strings list = new Strings(elements, buildPath(path));
        return (absolute && !windowsAbsolute ? "/" : "") + list.toString("/");
    }

    @Override
    public String toString() {
        return getPath();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof Path))
            return false;

        Path p = (Path) obj;

        return elements.equals(p.elements)
                && absolute == p.absolute
                && uri.equals(p.uri);
    }

    public String getFileNameWithoutExt() {
        String s = getFileName();
        if (!s.contains("."))
            return s;
        return s.substring(0, s.lastIndexOf("."));
    }

    public String getFileExt() {
        String s = getFileName();
        if (0 >= s.indexOf("."))
            return "";
        return s.substring(s.lastIndexOf(".") + 1);
    }

    public boolean isEmpty() {
        return elements.isEmpty();
    }

    public boolean isAbsolute() {
        return absolute;
    }

    public boolean isSame(String path) {
        if (path == null)
            return false;
        String s1 = new Path(path).toString();
        String s2 = toString();

        return caseSensitive ? s1.equals(s2) : s1.equalsIgnoreCase(s2);
    }

    public boolean endsWith(String... value) {
        return value != null ? endsWith(Arrays.asList(value)) : null;
    }

    public boolean endsWith(Iterable<String> value) {
        if (value == null)
            return false;

        String name = caseSensitive ? toString() : toString().toLowerCase();
        for (String s : value)
            if (s != null && name.endsWith(caseSensitive ? s : s.toLowerCase()))
                return true;

        return false;
    }

    public Path setRoot(Path root) {
        this.root = root;
        return this;
    }

    public Path getRoot() {
        return root;
    }

    /**
     * Zwraca rzeczywistą sciezkę do pliku. Jeśli parametr path jest ścieżką
     * bezwzględną, wtedy zwracana jest jego wartość, w przeciwnym razie jest
     * dodawany do bieżącej
     *
     * @param keystoreFile
     * @return
     */
    public File getReal(String path) {
        if (new Path(path).isAbsolute())
            return new File(path);
        return getFile(path);
    }

    public Path getRelativePath() {
        return getRelativePath(root);
    }

    public Path getRelativePath(Path parent) {
        Path result = new Path(this);
        if (parent == null)
            return result;

        int i = 0;
        while (i < parent.elements.size()) {
            String s = parent.elements.get(i);

            String d = result.elements.first(false);

            if (caseSensitive ? s.equals(d) : s.equalsIgnoreCase(d)) {
                result.elements.remove(0);
                result.absolute = false;
                result.windowsAbsolute = false;
                result.absolute = false;
            } else
                break;

            ++i;
        }

        return result;
    }

    @Override
    public Path clone() {
        return new Path(this);
    }

    public Path mkdirs() {
        getFile().mkdirs();
        return this;
    }

    public boolean matchesMask(String mask) {
        return Str.matchesMask(toString(), mask);
    }

    public Path changeExtension(String ext) {
        String last = elements.last(true);

        if (last.contains("."))
            last = last.substring(0, last.lastIndexOf("."));

        last += "." + ext;
        elements.add(last);
        return this;
    }

}

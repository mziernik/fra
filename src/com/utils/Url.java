package com.utils;

import com.exceptions.ThrowableException;
import com.mlogger.Log;
import com.servers.Connector;
import com.utils.text.StrWriter;
import com.servlet.controller.ControllerMetaData;
import com.servlet.controller.Controller;
import com.utils.collections.Params;
import com.utils.collections.Strings;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

public class Url implements Cloneable {

    private String protocol;
    private String username;
    private String password;
    private String hash;
    private String host;
    public String contextPath;
    private Integer port;
    private Strings path = new Strings().separator("/").nonEmpty(true);
    private boolean absolute = false;
    private boolean readOnly;
    private final Params params = new Params();
    private boolean endSlash;

    public Url(Class<? extends Controller> page) {
        this(ControllerMetaData.getUrl(page).toString());
    }

    public Url(Controller page) {
        this(ControllerMetaData.getUrl(page).toString());
    }

    //ToDo poprawic
    @ToDo("poprawic")
    public Url(Url url) {
        this(url != null ? url.toString() : null);
    }

    public Url(String url) {
        // jesli url == null, wtedy url zostanie potraktowany jako zbior parametrow (np post)

        url = Utils.coalesce(url, "").trim();

        absolute = url.startsWith("/");

        if (url.contains("#")) {
            hash = StrUtils.decodeURIComponent(url.substring(url.indexOf("#") + 1));
            url = url.substring(0, url.indexOf("#"));
        }

        if (url.contains("?")) {
            params.parseQuery(url.substring(url.indexOf("?") + 1));
            url = url.substring(0, url.indexOf("?"));
        }

        endSlash = url.trim().endsWith("/");

        if (url.contains("://"))
            try {
                URI u = new URI(url);
                protocol = u.getScheme();
                port = u.getPort() > 0 ? u.getPort() : null;
                String auth = u.getUserInfo();

                if (auth != null) {
                    username = auth;
                    if (auth.contains(":")) {
                        username = auth.substring(0, auth.indexOf(":"));
                        password = auth.substring(auth.indexOf(":") + 1, auth.length());
                    }
                }
                host = u.getHost();
                String p = u.getPath();
                if (p != null)
                    path.addAll(p.split("/"));

                String q = u.getQuery();
                if (q != null && q.contains("#"))
                    hash = q.substring(q.indexOf("#") + 1);

                return;

            } catch (URISyntaxException ex) {
                throw new ThrowableException(ex);
            }

        path.addAll(url.split("/"));
    }

    public Url(URI baseUri) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public Url readOnly() {
        readOnly = true;
        return this;
    }

    @Override
    public Url clone() {
        try {
            return (Url) super.clone();
        } catch (CloneNotSupportedException ex) {
            throw new ThrowableException(ex);
        }
    }

    public Strings path() {
        return path.clone();
    }

    public Path toPath(boolean includeHost) {
        return new Path()
                .absolute(true)
                .add(includeHost ? host : null)
                .add(this.path);
    }

    public boolean absolute() {
        return absolute;
    }

    public Url absolute(boolean absolute) {
        if (!readOnly)
            this.absolute = absolute;
        return this;
    }

    public Params params() {
        return readOnly ? params.clone() : params;
    }

    public String username() {
        return username;
    }

    public String password() {
        return password;
    }

    public String host() {
        return host;
    }

    public String protocol() {
        return protocol;
    }

    public Integer port() {
        return port;
    }

    public String hash() {
        return hash;
    }

    public Url hash(String hash) {
        if (!readOnly)
            this.hash = hash;
        return this;
    }

    public Url protocol(String protocol) {
        if (!readOnly)
            this.protocol = protocol;
        return this;
    }

    public Url host(String host) {
        if (!readOnly)
            this.host = host;
        return this;
    }

    public Url port(Integer port) {
        if (!readOnly)
            this.port = port;
        return this;
    }

    public Url username(String username) {
        if (!readOnly)
            this.username = username;
        return this;
    }

    public Url password(String password) {
        if (!readOnly)
            this.password = password;
        return this;
    }

    public Url param(Object name) {

        return readOnly ? this : param(Utils.toString(name), null, null);
    }

    public Url param(String name, Object value) {
        return readOnly ? this : param(name, value, null);
    }

    public Url param(String name, Object value, Boolean asText) {
        if (!readOnly)
            params.add(name, value);
        return this;
    }

    @Override
    public String toString() {
        StrWriter w = new StrWriter();

        if (protocol != null)
            w.append(protocol).append("://");

        if (username != null)
            w.append(username);
        if (password != null)
            w.append(":").append(password);

        if (username != null || password != null)
            w.append("@");

        String host = this.host;
        if (port != null)
            host += ":" + port;

        if (absolute)
            w.append("/");

        w.append(new Strings().nonEmpty(true)
                .add(host)
                .add(path)
                .toString("/"));

        if (endSlash)
            w.append("/");

        if (!params.isEmpty())
            w.append("?");

        if (!params.isEmpty())
            w.append(params.toURI());

        if (hash != null)
            w.append("#").append(hash);

        return w.toString();
    }

    public boolean isEmpty() {
        return path.isEmpty() && params.isEmpty() && hash == null;
    }

    @Override
    public boolean equals(Object obj) {
        return (obj instanceof Url) && toString().equals(obj.toString());
    }

    public URL getURL() {
        try {
            return new URL(toString());
        } catch (MalformedURLException ex) {
            throw new ThrowableException(ex);
        }
    }

    public Url getAbsolutePath(String path, Boolean https) {
        String s = "";
        String t = "";
        String qry = "";
        path = Utils.coalesce(path, "").trim();

        boolean isAbsolute = path.toLowerCase().startsWith("http://")
                || path.toLowerCase().startsWith("https://");

        if (isAbsolute) {
            path = path.substring(path.indexOf("://") + 3);
            if (path.contains("/"))
                path = path.substring(path.indexOf("/"));
        }

        if (path.contains("?")) {
            qry = path.substring(path.indexOf("?"));
            path = path.substring(0, path.indexOf("?"));
        }

        if (!path.startsWith("/"))
            s = "/";
        s += path.trim();

        Integer destPort = port;
        String destProtocol = protocol;

        Connector conn = Connector.get(https);
        if (conn != null) {
            destPort = conn.getPublicPort();
            destProtocol = conn.getScheme();
        }

        try {
            t = destPort != null
                    && destPort != 80
                    && destPort != 443
                            ? new URL(destProtocol,
                                    host,
                                    destPort,
                                    isAbsolute ? "" : Utils.coalesce(contextPath, "")).toString()
                            : new URL(destProtocol,
                                    host,
                                    isAbsolute ? "" : Utils.coalesce(contextPath, "")).toString();

        } catch (Exception ex) {
            Log.warning(ex);
        }

        return new Url((isAbsolute ? t + path : t + s) + qry);
    }

}

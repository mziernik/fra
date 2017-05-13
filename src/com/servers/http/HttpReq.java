package com.servers.http;

import com.html.core.Html;
import com.resources.dict.MimeMappings;
import com.io.IOUtils;
import com.sun.net.httpserver.*;
import com.utils.Utils;
import com.utils.collections.Params;
import com.utils.text.StrWriter;
import java.io.*;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URL;

public class HttpReq extends HttpExchange {

    public final Params headers = new Params().caseSensitive(false);
    public final Params params = new Params();
    private final HttpExchange http;
    public final String[] path;

    private boolean isCommited;

    public boolean isCommited() {
        return isCommited;
    }

    public HttpReq(final HttpExchange http) {
        this.http = http;

        String p = http.getRequestURI().getPath();
        if (p.startsWith("/"))
            p = p.substring(1);

        if (p.endsWith("/"))
            p = p.substring(0, p.length() - 1);
        path = p.split("\\/");

        params.parseQuery(http.getRequestURI().getQuery());
        headers.addAll(http.getRequestHeaders());
    }

    public HttpReq setHeader(String name, Object value) {
        http.getResponseHeaders().set(name, Utils.toString(value));
        return this;
    }

    public String getHeader(String name) {
        return http.getRequestHeaders().getFirst(name);
    }

    public String getHeaderF(String name) {
        String header = getHeader(name);
        if (header == null)
            throw new Error("Header \"" + name + "\" not found");
        return header;
    }

    @Override
    public Headers getRequestHeaders() {
        return http.getRequestHeaders();
    }

    @Override
    public Headers getResponseHeaders() {
        return http.getResponseHeaders();
    }

    @Override
    public URI getRequestURI() {
        return http.getRequestURI();
    }

    @Override
    public String getRequestMethod() {
        return http.getRequestMethod();
    }

    @Override
    public HttpContext getHttpContext() {
        return http.getHttpContext();
    }

    @Override
    public void close() {
        http.close();
    }

    @Override
    public InputStream getRequestBody() {
        return http.getRequestBody();
    }

    @Override
    public OutputStream getResponseBody() {
        isCommited = true;
        return http.getResponseBody();
    }

    @Override
    public void sendResponseHeaders(int i, long l) throws IOException {
        http.sendResponseHeaders(i, l);
    }

    @Override
    public InetSocketAddress getRemoteAddress() {
        return http.getRemoteAddress();
    }

    @Override
    public int getResponseCode() {
        return http.getResponseCode();
    }

    @Override
    public InetSocketAddress getLocalAddress() {
        return http.getLocalAddress();
    }

    @Override
    public String getProtocol() {
        return http.getProtocol();
    }

    @Override
    public Object getAttribute(String string) {
        return http.getAttribute(string);
    }

    @Override
    public void setAttribute(String string, Object o) {
        http.setAttribute(string, o);
    }

    @Override
    public void setStreams(InputStream in, OutputStream out) {
        isCommited = true;
        http.setStreams(in, out);
    }

    public int getContentLength() {
        String first = getRequestHeaders().getFirst("Content-Length");
        return Utils.strInt(first, -1);
    }

    @Override
    public HttpPrincipal getPrincipal() {
        return http.getPrincipal();
    }

    public void returnResource(URL url) throws IOException {
        Headers resp = getResponseHeaders();

        String file = url.getPath();

        String ext = file.contains(".")
                ? file.substring(file.lastIndexOf(".") + 1).toLowerCase() : "";

        for (String[] ss : MimeMappings.mappings)
            if (ss[0].equals(ext)) {
                resp.set("Content-Type", ss[1]);
                break;
            }
        try (InputStream in = new BufferedInputStream(url.openStream())) {
            sendResponseHeaders(200, in.available());
            try (OutputStream out = getResponseBody()) {
                IOUtils.copy(in, out);
            }
        }
    }

    public void returnHtml(Html html) throws IOException {

        StrWriter writer = new StrWriter();
        html.getContent(writer);

        byte[] data = writer.toString().getBytes(Utils.UTF8);

        setHeader("Content-Type", "text/html; charset=UTF-8");
        sendResponseHeaders(200, data.length);

        try (OutputStream resp = getResponseBody()) {
            resp.write(data);
        }
    }

}

package com.net.http;

import com.utils.Utils;
import com.utils.Is;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.nio.charset.Charset;

public class ContentSource {

    private final InputStream is;
    private final Reader reader;
    private final DataSource source;
    private final String string;
    Charset charset;
    String type;
    long length = -1;

    public ContentSource(DataSource source) {
        this.string = null;
        this.source = source;
        this.is = null;
        this.reader = null;
    }

    public ContentSource(String data) {
        this.string = data;
        this.source = null;
        this.is = null;
        this.reader = null;
    }

    public ContentSource(byte[] data) throws IOException {
        this(new ByteArrayInputStream(data));
    }

    public ContentSource(InputStream is) throws IOException {
        this.string = null;
        this.source = null;
        this.is = is;
        this.reader = null;
        this.length = is.available();
    }

    public ContentSource(Reader reader) {
        this.string = null;
        this.source = null;
        this.is = null;
        this.reader = reader;
    }

    public ContentSource charset(Charset charset) {
        this.charset = charset;
        return this;
    }

    public ContentSource type(String type) {
        this.type = type;
        return this;
    }

    public ContentSource length(long length) {
        this.length = length;
        return this;
    }

    void write(HttpRequest request, HttpURLConnection conn) throws IOException {

        if (Is.empty(conn.getRequestMethod()))
            conn.setRequestMethod("POST");

        if (Is.empty(type))
            type = request.getHeader("Content-Type");

        if (Is.empty(type))
            type = string != null || reader != null ? "text/plain" : "application/octetstream";

        if (!type.replace(" ", "").toLowerCase().contains(";charset=")) {
            if (charset != null)
                type += ";charset=" + charset.name();

            if (charset == null && "text/plain".equals(type))
                type += ";charset=UTF-8";
        }

        conn.setRequestProperty("Content-Type", type);

        String cl = length >= 0 ? Long.toString(length) : "";
        if (Is.empty(cl))
            cl = request.getHeader("Content-Length");
        if (!Is.empty(cl))
            conn.setRequestProperty("Content-Length", cl);

        try (OutputStream out = conn.getOutputStream()) {

            if (source != null) {
                source.write(out);
                out.flush();
                return;
            }
            int len;

            if (is != null) {
                byte[] buff = new byte[request.bufferSize];
                while ((len = is.read(buff)) > 0)
                    out.write(buff, 0, len);
                return;
            }

            if (string != null) {
                out.write(string.getBytes(Utils.coalesce(charset, Utils.UTF8)));
                return;
            }

            if (reader != null) {
                char[] buff = new char[request.bufferSize];
                while ((len = reader.read(buff)) > 0)
                    out.write(new String(buff, 0, len)
                            .getBytes(Utils.coalesce(charset, Utils.UTF8))
                    );
                return;
            }

        }

    }

}

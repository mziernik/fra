package com.net.http;

import com.utils.Utils;
import com.utils.Is;
import com.io.IOUtils;
import com.lang.LNet;
import com.mlogger.Log;
import com.utils.collections.Pairs;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.URLConnection;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

public class HttpResponse {

    private final HttpRequest req;
    private final HttpURLConnection conn;
    Pairs<String, String> headers;
    public final StackTraceElement[] sourceStackTrace;

    HttpResponse(StackTraceElement[] sourceStackTrace, HttpRequest req, HttpURLConnection conn) {
        this.req = req;
        this.conn = conn;
        this.sourceStackTrace = sourceStackTrace;
    }

    public String readStr(Charset charset) throws IOException {
        try (InputStream in = getInputStream()) {
            return IOUtils.read(in, Utils.coalesce(charset, Utils.UTF8));
        }
    }

    public String readStr() throws IOException {
        return readStr(charset());
    }

    public byte[] read() throws IOException {
        try (InputStream in = getInputStream()) {
            return IOUtils.read(in);
        }
    }

    public void read(OutputStream out) throws IOException {
        try (InputStream in = getInputStream()) {
            IOUtils.copy(in, out);
        }
    }

    public void read(Writer out) throws IOException {
        read(out, null);
    }

    public void read(Writer out, Charset charset) throws IOException {
        if (charset == null)
            charset = charset();
        if (charset == null)
            charset = Utils.UTF8;
        try (InputStream in = getInputStream()) {
            byte[] buff = new byte[req.bufferSize];
            int len;
            while ((len = in.read(buff)) > 0)
                out.write(new String(buff, 0, len, charset));
        }
    }

    public String contentDisposition() {
        String cd = conn.getHeaderField("Content-Disposition");
        if (cd != null && cd.indexOf("filename") > 0)
            try {
                cd = cd.substring(cd.indexOf("filename"), cd.length());
                cd = cd.substring(cd.indexOf("=") + 1, cd.length());

                if (cd.startsWith("\""))
                    cd = cd.substring(1, cd.length());

                if (cd.endsWith("\""))
                    cd = cd.substring(0, cd.length() - 1);

                cd = URLDecoder.decode(cd, "UTF-8");
            } catch (UnsupportedEncodingException ex) {
                Log.warning(ex);
            }
        return cd;
    }

    public Charset charset() {

        String ct = conn.getContentType();
        if (ct != null)
            for (String ss : ct.split(";")) {
                String[] split = ss.split("=");
                if (split.length == 2 && split[0].trim().equalsIgnoreCase("charset")) {
                    String chr = split[1].trim();

                    if (chr.startsWith("\"") && chr.endsWith("\""))
                        chr = chr.substring(1, chr.length() - 1);

                    if (chr.startsWith("'") && chr.endsWith("'"))
                        chr = chr.substring(1, chr.length() - 1);

                    if (Charset.isSupported(chr))
                        return Charset.forName(chr);
                }
            }
        return null;
    }

    public String contentType() {
        String ct = conn.getContentType();
        if (ct != null && ct.contains(";"))
            ct = ct.substring(0, ct.indexOf(";"));
        return ct;
    }

    public int code() throws IOException {
        return conn.getResponseCode();
    }

    public String message() throws IOException {
        return conn.getResponseMessage();
    }

    public String header(String name) {
        return conn.getHeaderField(name);
    }

    public Pairs<String, String> headers() {
        if (headers == null) {
            headers = new Pairs<>();
            for (Map.Entry<String, List<String>> en : conn.getHeaderFields().entrySet())
                if (en.getValue() != null)
                    for (String s : en.getValue())
                        headers.add(en.getKey(), s);
        }
        return headers;
    }

    public URLConnection connection() {
        return conn;
    }

    InputStream getInputStream() throws IOException {

        InputStream in = conn.getErrorStream();
        boolean err = in != null;

        if (in == null)
            in = conn.getInputStream();

        //    logResponse();
        String enc = conn.getContentEncoding();

        if (enc != null && !enc.isEmpty())
            switch (enc.toLowerCase().trim()) {
                case ("gzip"):
                    return new GZIPInputStream(in);
                default:
                    throw new IOException(LNet.INVALID_ENCODING.toString(enc));

            }
        return in;
    }

}

package com.net;

import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.cache.CachedData;
import com.config.CProxy;
import com.io.IOUtils;
import com.lang.LNet;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.utils.Url;
import com.utils.collections.Strings;
import com.utils.date.Timestamp;
import com.utils.hashes.Base64;
import com.xml.XML;
import java.io.*;
import java.net.*;
import java.nio.charset.Charset;
import java.util.*;
import java.util.Map.Entry;
import java.util.zip.GZIPInputStream;
import javax.net.ssl.*;

/**
 * Klasa ułatwiająca obsługę żądań HTTP. Obługuje kodowanie GZIP, metody POST,
 * GET dobierane automatycznie Miłosz Ziernik 2013/01/22
 */
@Deprecated
public class HttpClient implements Closeable {

    private final HttpURLConnection conn;
    private int connectTimeout = 3000;
    private int bufferSize = 1492;  // MTU
    private String boundary = "----------gPQhM7fvh9IuBxlofDVzXjZLVbIJa91bb7bk8tyk";
    public final Url url;
    private final Proxy proxy;

    public HttpClient(Url url) throws IOException {
        this(url, CProxy.getProxy(url.getURL()));
    }

    public HttpClient(String url) throws IOException {
        this(new Url(url), CProxy.getProxy(new Url(url).getURL()));
    }

    public HttpClient setConnectTimeout(int connectTimeout) {
        this.connectTimeout = connectTimeout;
        return this;
    }

    public HttpClient setReadTimeout(int readTimeout) {
        conn.setReadTimeout(readTimeout);
        return this;
    }

    public HttpClient setMultiPartBoundary(String boundary) {
        this.boundary = boundary;
        return this;
    }

    public HttpClient setBufferSize(int bufferSize) {
        this.bufferSize = bufferSize;
        return this;
    }

    public HttpClient(Url url, Proxy proxy) throws IOException {
        this.url = new Url(url);

        if (proxy == null)
            proxy = Proxy.NO_PROXY;

        this.proxy = proxy;

        conn = (HttpURLConnection) url.getURL().openConnection(proxy);

        if (proxy != Proxy.NO_PROXY)
            CProxy.setAuthorization(conn);
        conn.setRequestProperty("Content-Type", "application/octet-stream");
        conn.setRequestProperty("Accept-Encoding", "gzip");

        conn.setConnectTimeout(connectTimeout);

        conn.setDoOutput(true);
    }

    public HttpClient setAuthorization(String username, String password) {
        setHeader("Authorization", "Basic " + Base64.encode(
                Utils.coalesce(username, "") + ":" + Utils.coalesce(password)));
        return this;
    }

    public HttpClient markHostAsTrusted() {
        if (conn instanceof HttpsURLConnection)
            ((HttpsURLConnection) conn).setHostnameVerifier(new HostnameVerifier() {
                @Override
                public boolean verify(String hostname, SSLSession session) {
                    // dodaj biezaca nazwe hosta do zaufanych (SSL)
                    return (hostname.equals(url.host()));
                }
            });
        return this;
    }

    public HttpURLConnection getConnection() {
        return conn;
    }

    public HttpClient setContentType(String contentType, Charset charset) {
        setHeader("Content-Type", contentType + (charset != null ? "; charset=" + charset.name() : ""));
        return this;
    }

    public HttpClient setHeader(String name, String value) {
        conn.setRequestProperty(name, value);
        return this;
    }

    public int getResponseCode() throws IOException {
        logResponse();
        return conn.getResponseCode();
    }

    public String getResponseMessage() throws IOException {
        logResponse();
        return conn.getResponseMessage();
    }

    public HttpClient writeMultiPart(InputStream inputStream, Map<String, String> headers,
            String fieldName, String fileName, String contentType)
            throws IOException {

        conn.setRequestProperty("Content-Type",
                "multipart/form-data; boundary=" + boundary);

        StrWriter sb = new StrWriter();
        sb.append("--")
                .append(boundary)
                .append("\r\n");

        if (headers == null)
            headers = new LinkedHashMap<>();

        headers.put("Content-Type", contentType);

        sb.append("Content-Disposition: form-data; name=\"")
                .append(fieldName)
                .append("\"; filename=\"")
                .append(fileName)
                .append("\"\r\n");

        for (Entry<String, String> en : headers.entrySet())
            sb.append(en.getKey())
                    .append(":")
                    .append(en.getValue())
                    .append("\r\n");

        logRequest();

        sb.append("\r\n");
        try (OutputStream out = getOutputStream()) {
            out.write(sb.toString().getBytes());

            int len;
            byte[] buff = new byte[bufferSize];
            while ((len = inputStream.read(buff)) > 0)
                out.write(buff, 0, len);

            out.write(("\r\n--" + boundary + "--\r\n").getBytes());
            out.flush();
        }
        return this;
    }

    public HttpClient write(String string, Charset charset) throws IOException {
        if (charset == null)
            charset = Charset.forName("UTF-8");
        return write(new ByteArrayInputStream(string.getBytes(charset)));
    }

    public HttpClient write(XML xml) throws IOException {
        String ct = conn.getRequestProperty("Content-Type");
        if (ct == null || !ct.toLowerCase().contains("xml"))
            setContentType("text/xml", Utils.UTF8);
        try (OutputStreamWriter writer = new OutputStreamWriter(
                getOutputStream(), Utils.UTF8)) {
            xml.write(writer);
        }
        return this;
    }

    public HttpClient write(InputStream inputStream) throws IOException {
        if (inputStream == null)
            return this;

        logRequest();

        conn.setDoOutput(true);
        byte[] buff = new byte[bufferSize];
        int len;
        try (OutputStream out = getOutputStream()) {
            while ((len = inputStream.read(buff)) > 0)
                out.write(buff, 0, len);
            out.flush();
        }
        return this;
    }

    public InputStream getInputStream() throws IOException {

        logRequest();

        boolean ok = false;
        InputStream in;
        try {
            in = conn.getInputStream();
            ok = true;
        } catch (Exception e) {
            Log.warning("HTTP", e);
            in = conn.getErrorStream();
        }

        logResponse();

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

    public OutputStream getOutputStream() throws IOException {
        conn.setRequestMethod("POST");
        logRequest();
        conn.setDoOutput(true);
        return conn.getOutputStream();
    }

    public HttpClient read(OutputStream outputStream) throws IOException {
        if (outputStream == null)
            return this;

        InputStream in = getInputStream();
        if (in == null)
            return this;
        try {
            byte[] buff = new byte[bufferSize];
            int len;
            while ((len = in.read(buff)) > 0)
                outputStream.write(buff, 0, len);
        } finally {
            in.close();
        }
        outputStream.flush();
        return this;
    }

    public byte[] readBuff() throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        read(bout);
        return bout.toByteArray();
    }

    public Map<String, List<String>> getHeaderFields() {
        return conn.getHeaderFields();
    }

    public String getHeader(String header) {
        return conn.getHeaderField(header);
    }

    public String readStr() throws IOException {
        return readStr(null);
    }

    /**
     * Zwraca typ treści z nagłówka Content-Type. Usuwane są dodatkowe tresci
     * zapisane po średniku, np z "text/html;charset=utf-8" zwróci "text/html"
     *
     * @return
     */
    public String getContentType() {
        logResponse();
        String ct = conn.getContentType();
        if (ct != null && ct.contains(";"))
            ct = ct.substring(0, ct.indexOf(";"));
        return ct;
    }

    /**
     * Zwraca kodowanie treści z nagłówka Content-Encoding lub z Content-Type
     * (jesli zawiera atrybut charset)
     *
     * @return
     */
    public Charset getCharset() {
        String ct = conn.getContentType();
        if (ct != null) {

            SortedMap<String, Charset> availableCharsets = Charset.availableCharsets();

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
        }
        return null;
    }

    /**
     *
     * @param charset Kodowanie, może być NULLem, wtedy będzie pobierane z
     * odpowiedzi
     * @return
     * @throws IOException
     */
    public String readStr(Charset charset) throws IOException {
        try (InputStream in = getInputStream()) {
            charset = getCharset();

            if (charset == null)
                Log.warning("HTTP", "Brak deklaracji kodowania dla rezultatu żądania " + url);
            if (charset == null)
                charset = Charset.forName("UTF-8");
            return IOUtils.read(in, charset);
        }
    }

    public CachedData download() throws IOException {
        return download(null);
    }

    /**
     *
     * @param file może byc NULL-em
     * @return
     * @throws IOException
     */
    public CachedData download(CachedData file) throws IOException {
        String cd = getContentDisposition();
        String ct = Utils.coalesce(conn.getContentType(), "");
        String encoding = conn.getContentEncoding();

        if (file == null)
            file = new CachedData("HTTP", "download", Utils.coalesce(cd, "file"));
        else
            file.setName(Utils.coalesce(cd, "file"));

        file.attribute("URL", conn.getURL().toString());
        if (encoding != null)
            file.attribute("content-encoding", encoding);

        if (!ct.isEmpty())
            file.attribute("content-type", ct);

        read(file);

        return file;
    }

    public String getContentDisposition() {
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

    private boolean logRequest;
    private Timestamp reqTs;

    private void logRequest() {
        if (logRequest)
            return;
        logRequest = true;

        Log log = new Log(LogKind.DEBUG);
        log.tag("HTTP", conn.getRequestMethod(), "Request");
        log.value(conn.getURL().toString());

        Strings comment = new Strings();

        if (conn.usingProxy() && proxy != null && proxy != Proxy.NO_PROXY)
            comment.add("Proxy: " + proxy.toString());

        String ct = conn.getRequestProperty("Content-Type");
        if (ct != null) {
            if (ct.contains(";"))
                ct = ct.substring(0, ct.indexOf(";"));
            comment.add(ct);
        }

        comment.add(conn.getRequestProperty("Content-Encoding"));
        log.comment(comment.toString(", "));

        Map<String, List<String>> headers = conn.getRequestProperties();

        if (headers != null)
            for (Entry<String, List<String>> en : headers.entrySet())
                for (String s : en.getValue())
                    log.attribute("Header", en.getKey(), s);

        log.send();
        reqTs = new Timestamp();
    }

    private boolean logResponse;

    private void logResponse() {
        logRequest();
        if (logResponse)
            return;
        logResponse = true;

        Integer code = null;
        try {
            code = conn.getResponseCode();
        } catch (IOException ex) {
        }

        Log log = new Log(LogKind.DEBUG);
        log.tag("HTTP", "Response", code != null ? code.toString() : null);
        log.value(conn.getURL().toString());

        Strings comment = new Strings().nonEmpty(true);
        comment.add(getContentType());
        comment.add(conn.getContentEncoding());
        if (conn.getContentLengthLong() > 0)
            comment.add(Utils.formatSize(conn.getContentLengthLong()));

        comment.add(getContentDisposition());

        if (reqTs != null)
            comment.add(reqTs.diff().toString());

        log.comment(comment.toString(", "));

        Map<String, List<String>> headers = getHeaderFields();

        if (headers != null)
            for (Entry<String, List<String>> en : headers.entrySet())
                for (String s : en.getValue())
                    log.attribute("Header", en.getKey(), s);

        log.send();
    }

    @Override
    public void close() throws IOException {
        conn.disconnect();
    }

}

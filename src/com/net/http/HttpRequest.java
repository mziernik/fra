package com.net.http;

import com.utils.Utils;
import com.utils.Is;
import com.config.CProxy;
import com.exceptions.EError;
import com.servlet.interfaces.HttpMethod;
import com.utils.Url;
import com.utils.hashes.Base64;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.Proxy;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;

public class HttpRequest {

    private final HttpClient client;
    private final Url url;
    private final boolean async;
    private ContentSource content;
    private Proxy proxy;
    int connectTimeout = -1;
    int readTimeout = -1;
    private boolean trustedHost = false;
    HttpMethod method;
    int bufferSize = 1024 * 100;
    private boolean sent;

    private final Map<String, String> headers = new LinkedHashMap<>();

    HttpRequest(HttpClient client, Url url, boolean async) {
        this.client = client;
        this.url = url;
        this.async = async;

        this.connectTimeout = client.connectTimeout;
        this.readTimeout = client.readTimeout;
        headers.put("Accept-Encoding", "gzip");

        proxy = CProxy.getProxy(url.getURL());
    }

    public Url url() {
        return url;
    }

    public String getHeader(String name) {
        if (Is.empty(name))
            return null;
        for (Entry<String, String> en : headers.entrySet())
            if (en.getKey().equalsIgnoreCase(name))
                return en.getValue();
        return null;
    }

    public HttpRequest header(String name, Object value) {
        if (Is.empty(name) || Is.empty(value))
            return this;
        name = name.trim();
        for (String s : headers.keySet())
            if (s.equalsIgnoreCase(name)) {
                headers.remove(s);
                break;
            }
        headers.put(name, Utils.toString(value));
        return this;
    }

    public HttpRequest content(ContentSource content) {
        this.content = content;
        return this;
    }

    public HttpRequest connectTimeout(int connectTimeout) {
        this.connectTimeout = connectTimeout;
        return this;
    }

    public HttpRequest method(HttpMethod method) {
        this.method = method;
        return this;
    }

    public HttpRequest readTimeout(int readTimeout) {
        this.readTimeout = readTimeout;
        return this;
    }

    public HttpRequest proxy(Proxy proxy) {
        this.proxy = proxy;
        return this;
    }

    public HttpRequest setBasicAuthorization(String username, String password) {
        header("Authorization", "Basic " + Base64.encode(
                Utils.coalesce(username, "") + ":" + Utils.coalesce(password)));
        return this;
    }

    public HttpRequest markHostAsTrusted() {
        this.trustedHost = true;
        return this;
    }

    public void send(HttpResponseCallback callback) {
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        Objects.requireNonNull(callback);
        ExecutorService executor = client.getExecutor();
        sent = true;
        executor.submit(() -> {
            try {
                HttpResponse response = doSend(stack);
                callback.onResponse(response, null);
            } catch (Throwable ex) {
                IOException e = (ex instanceof IOException)
                        ? (IOException) ex
                        : new IOException(ex);
                callback.onResponse(null, EError.addDetails(e, "Source Stack Trace",
                        EError.stackTraceToString(stack).toString("\n")));
            }
        });
    }

    public HttpResponse send() throws IOException {
        return doSend(Thread.currentThread().getStackTrace());
    }

    private HttpResponse doSend(StackTraceElement[] stack) throws IOException {
        sent = true;
        Proxy proxy = Utils.coalesce(this.proxy, Proxy.NO_PROXY);
        HttpURLConnection conn = (HttpURLConnection) url.getURL()
                .openConnection(proxy);
        conn.setDoOutput(true);

        if (trustedHost && conn instanceof HttpsURLConnection)
            ((HttpsURLConnection) conn).setHostnameVerifier(
                    (String hostname, SSLSession session)
                    -> (hostname.equals(url.host())) // dodaj biezaca nazwe hosta do zaufanych (SSL)
            );

        if (connectTimeout > 0)
            conn.setConnectTimeout(connectTimeout);

        if (readTimeout > 0)
            conn.setReadTimeout(readTimeout);

        if (proxy != Proxy.NO_PROXY)
            CProxy.setAuthorization(conn);

        if (method != null)
            conn.setRequestMethod(method.name());

        for (Entry<String, String> en : headers.entrySet())
            conn.setRequestProperty(en.getKey(), en.getValue());

        // conn.setDoInput(true);
        if (content != null)
            content.write(this, conn);

        return new HttpResponse(stack, this, conn);
    }

}

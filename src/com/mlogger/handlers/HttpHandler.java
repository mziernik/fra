package com.mlogger.handlers;

import com.utils.Utils;
import com.utils.Is;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;
import com.mlogger.LogElement;

public class HttpHandler extends LogHandler {

    public final URL url;
    public Proxy proxy = Proxy.NO_PROXY;
    public int retryCount = 1;
    public int connectTimeout = 2000;
    public int readTimeout = 5000;

    public HttpHandler(URL url) {
        super();

        String prot = url.getProtocol();
        int port = url.getPort();

        String path = url.getPath();
        if (path == null)
            path = "";
        if (!path.endsWith("/"))
            path += "/";
        path += "api/addLog/";

        URL u = null;
        try {
            u = new URL(prot, url.getHost(), port, path);
        } catch (MalformedURLException ex) {
            Logger.getLogger(HttpHandler.class.getName()).log(Level.SEVERE, null, ex);
        }

        this.url = u;
    }

    @Override
    public boolean equals(Object o) {
        return (o instanceof HttpHandler) && url.equals(((HttpHandler) o).url);
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 23 * hash + (this.url != null ? this.url.hashCode() : 0);
        return hash;
    }

    @Override
    public String toString() {
        return url != null ? url.toString() : null;
    }

    @Override
    public void publish(LogElement log, LinkedList<Handler> handlers, LogRecord record) throws Exception {

        byte[] data = prepareData(log);

        if (url == null)
            return;

        HttpURLConnection conn = (HttpURLConnection) url.openConnection(
                proxy != null ? proxy : Proxy.NO_PROXY);
        conn.setConnectTimeout(connectTimeout);
        conn.setReadTimeout(readTimeout);
        conn.setRequestProperty("Content-Type", "application/octet-stream");

        if (conn instanceof HttpsURLConnection)
            ((HttpsURLConnection) conn).setHostnameVerifier(new HostnameVerifier() {
                @Override
                public boolean verify(String hostname, SSLSession session) {
                    // dodaj biezaca nazwe hosta do zaufanych
                    return (hostname.equals(url.getHost()));
                }
            });

        conn.setDoOutput(true);
        conn.setDoInput(true);
        OutputStream out = conn.getOutputStream();
        try {
            out.write(data);
            out.flush();
        } finally {
            out.close();
        }

        int status = conn.getResponseCode();
        String respMsg = conn.getResponseMessage();

        String ct = conn.getContentType();

        InputStream in = null;
        byte[] buff = new byte[10240];
        int len;
        try {

            try {
                in = conn.getInputStream();
            } catch (Exception e) {
                in = conn.getErrorStream();
            }

            ByteArrayOutputStream bout = new ByteArrayOutputStream();

            while (in != null && (len = in.read(buff)) > 0)
                bout.write(buff, 0, len);

            String line = new String(bout.toByteArray(), Utils.UTF8).trim();
            processResponse(log, line);
        } finally {
            if (in != null)
                in.close();
        }

        if (status != 200)
            throw new ServerException("HTTP " + status + ": " + respMsg);
    }

}

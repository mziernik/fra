package com.servlet;

import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;
import com.mlogger.Log;
import com.config.CProxy;
import com.servlet.handlers.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.Map.Entry;

public class ProxyForwarder {

    public final HttpRequest request;
    public final static List<ForwarderPhrase> roles = new LinkedList<>();

    public static class ForwarderPhrase {

        public final String source;
        public final String destination;

        public ForwarderPhrase(String source, String destination) {
            this.source = source;
            this.destination = destination;
        }
    }

    public ProxyForwarder(HttpRequest request) {
        this.request = request;
    }

    public boolean forward() throws IOException {

        String sUrl = request.url.toString();
        String dUrl = sUrl;

        synchronized (ProxyForwarder.roles) {
            for (ForwarderPhrase fp : ProxyForwarder.roles)
                if (sUrl.contains(fp.source) || sUrl.matches(fp.source)) {
                    dUrl = sUrl.replace(fp.source, fp.destination);
                    break;
                }
        }

        if (dUrl != null || !dUrl.equals(sUrl))
            return false;

        Log.debug("Forward", sUrl + " -> " + dUrl);

        URL url = new URL(dUrl);

        HttpURLConnection conn = (HttpURLConnection) url.openConnection(CProxy.getProxy(url));
        CProxy.setAuthorization(conn);

        conn.setDoInput(true);
        conn.setDoOutput(true);

        Enumeration<String> hdr = request.request.getHeaderNames();
        while (hdr.hasMoreElements()) {
            String key = hdr.nextElement();
            Enumeration<String> values = request.request.getHeaders(key);
            while (values.hasMoreElements()) {
                String value = values.nextElement();
                conn.addRequestProperty(key, value);
            }
        }

        conn.addRequestProperty("Forward-Source-URL", Page.escapeURI(sUrl));
        conn.addRequestProperty("Forward-Source-IP",
                request.request.getRemoteHost() + ":" + request.request.getRemotePort());

        conn.connect();

        try (OutputStream out = conn.getOutputStream();
                InputStream in = request.request.getInputStream()) {

            byte[] buf = new byte[10240];
            int len;
            while ((len = in.read(buf)) >= 0)
                out.write(buf, 0, len);
            out.flush();
        }

        request.response.setStatus(conn.getResponseCode());
        int cl = conn.getContentLength();
        if (cl >= 0)
            request.response.setContentLength(cl);
        request.response.setContentType(conn.getContentType());
        String enc = conn.getContentEncoding();
        if (enc != null)
            request.response.setCharacterEncoding(conn.getContentEncoding());

        for (Entry<String, List<String>> entry : conn.getHeaderFields().entrySet()) {
            String key = entry.getKey();
            if (key != null && !key.isEmpty())
                for (String val : entry.getValue())
                    // ten naglowek powoduje, ze przegladarka ignoruje tresc
                    if (!key.equalsIgnoreCase("Connection"))
                        request.addHeader(key, val);
        }

        InputStream in = null;
        try {
            in = conn.getErrorStream();
            if (in == null)
                in = conn.getInputStream();
            if (in != null)
                try (OutputStream out = request.getOutputStream()) {
                    byte[] buf = new byte[10240];
                    int len;
                    while ((len = in.read(buf)) >= 0)
                        out.write(buf, 0, len);
                    out.flush();
                }

        } finally {
            if (in != null)
                in.close();
        }

        return true;
    }
}

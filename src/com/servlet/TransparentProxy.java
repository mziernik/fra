package com.servlet;

import com.io.IOUtils;
import com.io.TOutputStream;
import com.mlogger.Log;
import com.utils.Is;
import com.utils.console.TConsole;
import com.utils.date.Timestamp;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.Map.Entry;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class TransparentProxy {

    public final static Map<String, TransparentProxyMapping> mappings = new LinkedHashMap<>();

    public static TransparentProxyMapping register(String prefix, String toUrl) {
        return new TransparentProxyMapping(prefix, toUrl);
    }

    static boolean process(HttpServletRequest req, HttpServletResponse resp) throws MalformedURLException, IOException {
        if (mappings.isEmpty())
            return false;

        String ctx = req.getRequestURI();

        TransparentProxyMapping mapping = null;

        URL url = new URL(req.getRequestURL().toString());
        String host = url.getProtocol() + "://" + url.getHost() + ":" + url.getPort();
        String file = url.getFile();
        String query = req.getQueryString() != null ? "?" + req.getQueryString() : "";

        for (TransparentProxyMapping tpm : mappings.values())
            if (ctx.startsWith(tpm.prefix)) {
                mapping = tpm;
                break;
            }

        if (mapping == null) {
            String ref = req.getHeader("referer");
            if (ref != null) {
                ref = new URL(ref).getFile();

                for (TransparentProxyMapping tpm : mappings.values())
                    if (ref.startsWith(tpm.prefix)) {
                        mapping = tpm;
                        break;
                    }

                if (mapping != null)
                    if (!file.startsWith(mapping.prefix)) {
                        resp.setStatus(307); // przekierowanie wraz z postem
                        resp.setHeader("Location",
                                host + mapping.prefix + file + query);
                        return true;
                    }
            }

        }

        if (mapping == null)
            return false;

        try {
            mapping.process(req, resp, host, file);
        } catch (Throwable e) {
            Log.warning(e);
            MainServlet.printError(e, resp);
            return false;
        }
        return true;
    }

    public static class TransparentProxyMapping {

        public final String prefix;
        public final String toUrl;
        public final Set<String> hopHeaders = new HashSet<>();

        private TransparentProxyMapping(String prefix, String toUrl) {
            if (!prefix.startsWith("/"))
                prefix = "/" + prefix;

            this.prefix = prefix;
            this.toUrl = toUrl;
            hopHeaders.add("connection");
            hopHeaders.add("keep-alive");
            hopHeaders.add("proxy-authorization");
            hopHeaders.add("proxy-authenticate");
            hopHeaders.add("proxy-connection");
            hopHeaders.add("transfer-encoding");
            hopHeaders.add("te");
            hopHeaders.add("trailer");
            hopHeaders.add("upgrade");
            mappings.put(prefix, this);
        }

        public String getDestination(HttpServletRequest request) {
            String src = request.getRequestURL().toString();

            src = src.substring(src.indexOf("://") + 3);
            src = src.substring(src.indexOf("/"));
            if (src.startsWith(prefix))
                src = src.substring(prefix.length());

            return src;
        }

        private void process(HttpServletRequest request, HttpServletResponse response,
                String host, String destination)
                throws Exception {

            Timestamp ts = new Timestamp();

            String src = getDestination(request);
            //CProxy.getProxy(url)
            URL url = new URL(toUrl + src);

            HttpURLConnection conn = (HttpURLConnection) url.openConnection(Proxy.NO_PROXY);

            conn.setUseCaches(false);

            Log.info("Proxy", request.getRequestURL() + " -> " + url);

            conn.setRequestMethod(request.getMethod());

            boolean data = request.getContentLength() > 0 && !Is.empty(request.getContentType());
            conn.setDoOutput(data);
            conn.setDoInput(true);

            if (conn instanceof HttpsURLConnection)
                ((HttpsURLConnection) conn).setHostnameVerifier(
                        (String hostname, SSLSession session)
                        -> (hostname.equals(url.getHost())) // dodaj biezaca nazwe hosta do zaufanych (SSL)
                );

            for (Enumeration<String> headerNames = request.getHeaderNames(); headerNames.hasMoreElements();) {
                String headerName = headerNames.nextElement();
                if (hopHeaders.contains(headerName.toLowerCase()))
                    continue;

                for (Enumeration<String> headers = request.getHeaders(headerName); headers.hasMoreElements();)
                    conn.addRequestProperty(headerName, headers.nextElement());
            }

            conn.setRequestProperty("host", getDomain(toUrl));
            conn.setRequestProperty("connection", "keep-alive");

            conn.setRequestProperty("X-Forwarded-For", request.getRemoteAddr());

            if (data) {
                conn.setFixedLengthStreamingMode(request.getContentLength());

                try (ServletInputStream rin = request.getInputStream();
                        OutputStream cout = conn.getOutputStream();) {

                    int available = rin.available();

                    TConsole.print(src + ": " + available);
                    TOutputStream tOutputStream = new TOutputStream(true, cout);
                    IOUtils.copy(rin, tOutputStream);
                    TConsole.print(new String(tOutputStream.memory()));
                }
            }

            long ct = System.currentTimeMillis();

            conn.connect();

            boolean ok = false;
            InputStream in;
            try {
                in = conn.getInputStream();
                ok = true;
            } catch (Exception e) {
                Log.warning("HTTP", e);
                in = conn.getErrorStream();
            }

            int responseCode = conn.getResponseCode();
            response.setStatus(responseCode);

            for (Entry<String, List<String>> en : conn.getHeaderFields().entrySet()) {
                if (en.getKey() == null || en.getValue() == null)
                    continue;
                for (String s : en.getValue())
                    response.addHeader(en.getKey(), s);

            }

            ct = System.currentTimeMillis() - ct;
            long cb = System.currentTimeMillis();

            if (responseCode == 302 || responseCode == 307) {
                String location = conn.getHeaderField("location");

                if (location != null) {

                    String protocol = location.substring(0, location.indexOf("://"));
                    location = host + prefix + location.substring(
                            location.indexOf("/", location.indexOf("://") + 3));

                    response.setHeader("Location", location);

                }

            }
            if (in != null)
                try (BufferedOutputStream out = new BufferedOutputStream(response.getOutputStream(), 1024 * 100)) {
                    IOUtils.copy(new BufferedInputStream(in, 1024 * 100), out);
                    out.flush();
                    in.close();
                }

            cb = System.currentTimeMillis() - cb;

            if (destination.contains(".jpg") || destination.contains(".png"))
                ts.console(ct + "   " + cb + "   " + destination);

        }

        private String getDomain(String url) {
            if (!url.contains("://"))
                return null;
            url = url.substring(url.indexOf("://") + 3);

            if (url.contains("/"))
                return url.substring(0, url.indexOf("/"));
            else
                return url;
        }
    }

}

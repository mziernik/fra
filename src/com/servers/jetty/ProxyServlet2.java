package com.servers.jetty;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.servlet.AsyncContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.UnavailableException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.client.api.ContentProvider;
import org.eclipse.jetty.client.api.Request;
import org.eclipse.jetty.client.api.Response;
import org.eclipse.jetty.client.api.Result;
import org.eclipse.jetty.client.util.InputStreamContentProvider;
import org.eclipse.jetty.http.HttpField;
import org.eclipse.jetty.http.HttpHeader;
import org.eclipse.jetty.http.HttpHeaderValue;
import org.eclipse.jetty.http.HttpStatus;
import org.eclipse.jetty.http.HttpVersion;
import org.eclipse.jetty.util.Callback;
import org.eclipse.jetty.util.HttpCookieStore;
import org.eclipse.jetty.util.log.Log;
import org.eclipse.jetty.util.log.Logger;
import org.eclipse.jetty.util.thread.QueuedThreadPool;

public class ProxyServlet2 extends HttpServlet {

    protected static final Set<String> HOP_HEADERS;

    static {
        Set<String> hopHeaders = new HashSet<>();
        hopHeaders.add("connection");
        hopHeaders.add("keep-alive");
        hopHeaders.add("proxy-authorization");
        hopHeaders.add("proxy-authenticate");
        hopHeaders.add("proxy-connection");
        hopHeaders.add("transfer-encoding");
        hopHeaders.add("te");
        hopHeaders.add("trailer");
        hopHeaders.add("upgrade");
        HOP_HEADERS = Collections.unmodifiableSet(hopHeaders);
    }

    private final Set<String> _whiteList = new HashSet<>();
    private final Set<String> _blackList = new HashSet<>();

    public boolean preserveHost;
    public String hostHeader;
    public String viaHost;
    private static HttpClient _client;
    private long _timeout;

    public String proxyTo;
    public String prefix;

    static Executor executor;

    protected String rewriteTarget(HttpServletRequest request) {
        String path = request.getRequestURI();
        if (!path.startsWith(prefix))
            return null;

        StringBuilder uri = new StringBuilder(proxyTo);
        if (proxyTo.endsWith("/"))
            uri.setLength(uri.length() - 1);
        String rest = path.substring(prefix.length());
        if (!rest.isEmpty()) {
            if (!rest.startsWith("/"))
                uri.append("/");
            uri.append(rest);
        }

        String query = request.getQueryString();
        if (query != null) {
            // Is there at least one path segment ?
            String separator = "://";
            if (uri.indexOf("/", uri.indexOf(separator) + separator.length()) < 0)
                uri.append("/");
            uri.append("?").append(query);
        }
        URI rewrittenURI = URI.create(uri.toString()).normalize();

        if (!validateDestination(rewrittenURI.getHost(), rewrittenURI.getPort()))
            return null;

        return rewrittenURI.toString();
    }

    @Override
    protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

        if (_client == null)
            _client = createHttpClient();

        final int requestId = getRequestId(request);

        String rewrittenTarget = rewriteTarget(request);

        if (rewrittenTarget == null) {
            onProxyRewriteFailed(request, response);
            return;
        }

        final Request proxyRequest = getHttpClient().newRequest(rewrittenTarget)
                .method(request.getMethod())
                .version(HttpVersion.fromString(request.getProtocol()));

        copyRequestHeaders(request, proxyRequest);

        addProxyHeaders(request, proxyRequest);

        final AsyncContext asyncContext = request.startAsync();
        // We do not timeout the continuation, but the proxy request
        asyncContext.setTimeout(0);
        proxyRequest.timeout(getTimeout(), TimeUnit.MILLISECONDS);

        if (hasContent(request))
            proxyRequest.content(proxyRequestContent(request, response, proxyRequest));

        sendProxyRequest(request, response, proxyRequest);
    }

    protected ContentProvider proxyRequestContent(HttpServletRequest request, HttpServletResponse response, Request proxyRequest) throws IOException {
        return new ProxyInputStreamContentProvider(request, response, proxyRequest, request.getInputStream());
    }

    protected Response.Listener newProxyResponseListener(HttpServletRequest request, HttpServletResponse response) {
        return new ProxyResponseListener(request, response);
    }

    protected void onResponseContent(HttpServletRequest request, HttpServletResponse response, Response proxyResponse, byte[] buffer, int offset, int length, Callback callback) {
        try {
            response.getOutputStream().write(buffer, offset, length);
            callback.succeeded();
        } catch (Throwable x) {
            callback.failed(x);
        }
    }

    protected class ProxyResponseListener extends Response.Listener.Adapter {

        private final HttpServletRequest request;
        private final HttpServletResponse response;

        protected ProxyResponseListener(HttpServletRequest request, HttpServletResponse response) {
            this.request = request;
            this.response = response;
        }

        @Override
        public void onBegin(Response proxyResponse) {
            response.setStatus(proxyResponse.getStatus());
        }

        @Override
        public void onHeaders(Response proxyResponse) {
            onServerResponseHeaders(request, response, proxyResponse);
        }

        @Override
        public void onContent(final Response proxyResponse, ByteBuffer content, final Callback callback) {
            byte[] buffer;
            int offset;
            int length = content.remaining();
            if (content.hasArray()) {
                buffer = content.array();
                offset = content.arrayOffset();
            } else {
                buffer = new byte[length];
                content.get(buffer);
                offset = 0;
            }

            onResponseContent(request, response, proxyResponse, buffer, offset, length, new Callback.Nested(callback) {
                @Override
                public void failed(Throwable x) {
                    super.failed(x);
                    proxyResponse.abort(x);
                }
            });
        }

        @Override
        public void onComplete(Result result) {
            if (result.isSucceeded())
                onProxyResponseSuccess(request, response, result.getResponse());
            else
                onProxyResponseFailure(request, response, result.getResponse(), result.getFailure());
        }
    }

    protected class ProxyInputStreamContentProvider extends InputStreamContentProvider {

        private final HttpServletResponse response;
        private final Request proxyRequest;
        private final HttpServletRequest request;

        protected ProxyInputStreamContentProvider(HttpServletRequest request, HttpServletResponse response, Request proxyRequest, InputStream input) {
            super(input);
            this.request = request;
            this.response = response;
            this.proxyRequest = proxyRequest;
        }

        @Override
        public long getLength() {
            return request.getContentLength();
        }

        @Override
        protected ByteBuffer onRead(byte[] buffer, int offset, int length) {
            return onRequestContent(request, proxyRequest, buffer, offset, length);
        }

        protected ByteBuffer onRequestContent(HttpServletRequest request, Request proxyRequest, byte[] buffer, int offset, int length) {
            return super.onRead(buffer, offset, length);
        }

        @Override
        protected void onReadFailure(Throwable failure) {
            onClientRequestFailure(request, proxyRequest, response, failure);
        }
    }

    @Override
    public void destroy() {
        try {
            _client.stop();
        } catch (Exception x) {
            x.printStackTrace();
        }
    }

    public String getHostHeader() {
        return hostHeader;
    }

    public String getViaHost() {
        return viaHost;
    }

    private static String viaHost() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException x) {
            return "localhost";
        }
    }

    public long getTimeout() {
        return _timeout;
    }

    public void setTimeout(long timeout) {
        this._timeout = timeout;
    }

    public Set<String> getWhiteListHosts() {
        return _whiteList;
    }

    public Set<String> getBlackListHosts() {
        return _blackList;
    }

    protected HttpClient createHttpClient() throws ServletException {

        HttpClient client = newHttpClient();

        // Redirects must be proxied as is, not followed.
        client.setFollowRedirects(false);

        // Must not store cookies, otherwise cookies of different clients will mix.
        client.setCookieStore(new HttpCookieStore.Empty());

        if (executor == null)
            throw new IllegalStateException("No server executor for proxy");

        client.setExecutor(executor);

//        value = config.getInitParameter("maxConnections");
//        if (value == null)
//            value = "256";
        client.setMaxConnectionsPerDestination(256);

//        value = config.getInitParameter("idleTimeout");
//        if (value == null)
//            value = "30000";
        client.setIdleTimeout(30000);

//        value = config.getInitParameter("timeout");
//        if (value == null)
//            value = "";
        _timeout = 60000;

//        value = config.getInitParameter("requestBufferSize");
//        if (value != null)
//            client.setRequestBufferSize(Integer.parseInt(value));
//        value = config.getInitParameter("responseBufferSize");
//        if (value != null)
//            client.setResponseBufferSize(Integer.parseInt(value));
        try {
            client.start();

            // Content must not be decoded, otherwise the client gets confused.
            client.getContentDecoderFactories().clear();

            // No protocol handlers, pass everything to the client.
            client.getProtocolHandlers().clear();

            return client;
        } catch (Exception x) {
            throw new ServletException(x);
        }
    }

    /**
     * @return a new HttpClient instance
     */
    protected HttpClient newHttpClient() {
        return new HttpClient();
    }

    protected HttpClient getHttpClient() {
        return _client;
    }

    private Set<String> parseList(String list) {
        Set<String> result = new HashSet<>();
        String[] hosts = list.split(",");
        for (String host : hosts) {
            host = host.trim();
            if (host.length() == 0)
                continue;
            result.add(host);
        }
        return result;
    }

    /**
     * Checks the given {@code host} and {@code port} against whitelist and
     * blacklist.
     *
     * @param host the host to check
     * @param port the port to check
     * @return true if it is allowed to be proxy to the given host and port
     */
    public boolean validateDestination(String host, int port) {
        String hostPort = host + ":" + port;
        if (!_whiteList.isEmpty())
            if (!_whiteList.contains(hostPort))
                return false;
        if (!_blackList.isEmpty())
            if (_blackList.contains(hostPort))
                return false;
        return true;
    }

    protected void onProxyRewriteFailed(HttpServletRequest clientRequest, HttpServletResponse proxyResponse) {
        sendProxyResponseError(clientRequest, proxyResponse, HttpStatus.FORBIDDEN_403);
    }

    protected boolean hasContent(HttpServletRequest clientRequest) {
        return clientRequest.getContentLength() > 0
                || clientRequest.getContentType() != null
                || clientRequest.getHeader(HttpHeader.TRANSFER_ENCODING.asString()) != null;
    }

    protected void copyRequestHeaders(HttpServletRequest clientRequest, Request proxyRequest) {
        // First clear possibly existing headers, as we are going to copy those from the client request.
        proxyRequest.getHeaders().clear();

        Set<String> headersToRemove = findConnectionHeaders(clientRequest);

        for (Enumeration<String> headerNames = clientRequest.getHeaderNames(); headerNames.hasMoreElements();) {
            String headerName = headerNames.nextElement();
            String lowerHeaderName = headerName.toLowerCase(Locale.ENGLISH);

            if (HttpHeader.HOST.is(headerName) && !preserveHost)
                continue;

            // Remove hop-by-hop headers.
            if (HOP_HEADERS.contains(lowerHeaderName))
                continue;
            if (headersToRemove != null && headersToRemove.contains(lowerHeaderName))
                continue;

            for (Enumeration<String> headerValues = clientRequest.getHeaders(headerName); headerValues.hasMoreElements();) {
                String headerValue = headerValues.nextElement();
                if (headerValue != null)
                    proxyRequest.header(headerName, headerValue);
            }
        }

        // Force the Host header if configured
        if (hostHeader != null)
            proxyRequest.header(HttpHeader.HOST, hostHeader);
    }

    protected Set<String> findConnectionHeaders(HttpServletRequest clientRequest) {
        // Any header listed by the Connection header must be removed:
        // http://tools.ietf.org/html/rfc7230#section-6.1.
        Set<String> hopHeaders = null;
        Enumeration<String> connectionHeaders = clientRequest.getHeaders(HttpHeader.CONNECTION.asString());
        while (connectionHeaders.hasMoreElements()) {
            String value = connectionHeaders.nextElement();
            String[] values = value.split(",");
            for (String name : values) {
                name = name.trim().toLowerCase(Locale.ENGLISH);
                if (hopHeaders == null)
                    hopHeaders = new HashSet<>();
                hopHeaders.add(name);
            }
        }
        return hopHeaders;
    }

    protected void addProxyHeaders(HttpServletRequest clientRequest, Request proxyRequest) {
        addViaHeader(proxyRequest);
        addXForwardedHeaders(clientRequest, proxyRequest);
    }

    protected void addViaHeader(Request proxyRequest) {
        proxyRequest.header(HttpHeader.VIA, "http/1.1 " + getViaHost());
    }

    protected void addXForwardedHeaders(HttpServletRequest clientRequest, Request proxyRequest) {
        proxyRequest.header(HttpHeader.X_FORWARDED_FOR, clientRequest.getRemoteAddr());
        proxyRequest.header(HttpHeader.X_FORWARDED_PROTO, clientRequest.getScheme());
        proxyRequest.header(HttpHeader.X_FORWARDED_HOST, clientRequest.getHeader(HttpHeader.HOST.asString()));
        proxyRequest.header(HttpHeader.X_FORWARDED_SERVER, clientRequest.getLocalName());
    }

    protected void sendProxyRequest(HttpServletRequest clientRequest, HttpServletResponse proxyResponse, Request proxyRequest) {
        proxyRequest.send(newProxyResponseListener(clientRequest, proxyResponse));
    }

    protected void onClientRequestFailure(HttpServletRequest clientRequest, Request proxyRequest, HttpServletResponse proxyResponse, Throwable failure) {
        boolean aborted = proxyRequest.abort(failure);
        if (!aborted) {
            int status = failure instanceof TimeoutException
                    ? HttpStatus.REQUEST_TIMEOUT_408
                    : HttpStatus.INTERNAL_SERVER_ERROR_500;
            sendProxyResponseError(clientRequest, proxyResponse, status);
        }
    }

    protected void onServerResponseHeaders(HttpServletRequest clientRequest, HttpServletResponse proxyResponse, Response serverResponse) {
        for (HttpField field : serverResponse.getHeaders()) {
            String headerName = field.getName();
            String lowerHeaderName = headerName.toLowerCase(Locale.ENGLISH);
            if (HOP_HEADERS.contains(lowerHeaderName))
                continue;

            String newHeaderValue = filterServerResponseHeader(clientRequest, serverResponse, headerName, field.getValue());
            if (newHeaderValue == null || newHeaderValue.trim().length() == 0)
                continue;

            proxyResponse.addHeader(headerName, newHeaderValue);
        }
    }

    protected String filterServerResponseHeader(HttpServletRequest clientRequest, Response serverResponse, String headerName, String headerValue) {
        return headerValue;
    }

    protected void onProxyResponseSuccess(HttpServletRequest clientRequest, HttpServletResponse proxyResponse, Response serverResponse) {
        AsyncContext asyncContext = clientRequest.getAsyncContext();
        asyncContext.complete();
    }

    protected void onProxyResponseFailure(HttpServletRequest clientRequest, HttpServletResponse proxyResponse, Response serverResponse, Throwable failure) {
        if (proxyResponse.isCommitted())
            try {
                // Use Jetty specific behavior to close connection.
                proxyResponse.sendError(-1);
                AsyncContext asyncContext = clientRequest.getAsyncContext();
                asyncContext.complete();
            } catch (IOException x) {
                x.printStackTrace();
            }
        else {
            proxyResponse.resetBuffer();
            int status = failure instanceof TimeoutException
                    ? HttpStatus.GATEWAY_TIMEOUT_504
                    : HttpStatus.BAD_GATEWAY_502;
            sendProxyResponseError(clientRequest, proxyResponse, status);
        }
    }

    protected int getRequestId(HttpServletRequest clientRequest) {
        return System.identityHashCode(clientRequest);
    }

    protected void sendProxyResponseError(HttpServletRequest clientRequest, HttpServletResponse proxyResponse, int status) {
        proxyResponse.setStatus(status);
        proxyResponse.setHeader(HttpHeader.CONNECTION.asString(), HttpHeaderValue.CLOSE.asString());
        if (clientRequest.isAsyncStarted())
            clientRequest.getAsyncContext().complete();
    }

}

package com.servlet.webservices.jax;

import com.utils.text.StrWriter;
import com.sun.istack.NotNull;
import com.sun.xml.ws.api.ha.HaInfo;
import com.sun.xml.ws.api.message.Packet;
import com.sun.xml.ws.api.server.PortAddressResolver;
import com.sun.xml.ws.api.server.WSEndpoint;
import com.sun.xml.ws.api.server.WebServiceContextDelegate;
import com.sun.xml.ws.resources.WsservletMessages;
import com.sun.xml.ws.transport.Headers;
import com.sun.xml.ws.transport.http.HttpAdapter;
import com.sun.xml.ws.transport.http.WSHTTPConnection;
import com.sun.xml.ws.developer.JAXWSProperties;

import javax.servlet.ServletContext;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.handler.MessageContext;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * {@link WSHTTPConnection} implemented for {@link HttpServlet}.
 *
 * @author WS Development Team
 */
public class ServletConnectionImpl extends WSHTTPConnection implements WebServiceContextDelegate {

    private final HttpServletRequest request;
    private final HttpServletResponse response;
    private final ServletContext context;
    private int status;
    private Headers requestHeaders;
    private final ServletAdapter adapter;
    private Headers responseHeaders;
    private HaInfo haInfo;

    public ServletConnectionImpl(@NotNull ServletAdapter adapter, ServletContext context, HttpServletRequest request, HttpServletResponse response) {
        this.adapter = adapter;
        this.context = context;
        this.request = request;
        this.response = response;
    }

    @Override
    @Property({MessageContext.HTTP_REQUEST_HEADERS, Packet.INBOUND_TRANSPORT_HEADERS})
    public @NotNull
    Map<String, List<String>> getRequestHeaders() {
        if (requestHeaders == null) {
            requestHeaders = new Headers();
            Enumeration enums = request.getHeaderNames();
            while (enums.hasMoreElements()) {
                String headerName = (String) enums.nextElement();
                Enumeration e = request.getHeaders(headerName);
                if (e != null) {
                    List<String> values = requestHeaders.get(headerName);
                    while (e.hasMoreElements()) {
                        String headerValue = (String) e.nextElement();
                        if (values == null) {
                            values = new ArrayList<String>();
                            requestHeaders.put(headerName, values);
                        }
                        values.add(headerValue);
                    }
                }
            }
        }
        return requestHeaders;
    }

    @Override
    public Set<String> getRequestHeaderNames() {
        return getRequestHeaders().keySet();
    }

    @Override
    public List<String> getRequestHeaderValues(@NotNull String headerName) {
        if (requestHeaders != null)
            return requestHeaders.get(headerName);
        return null;
    }

    /**
     * sets response headers.
     */
    @Override
    public void setResponseHeaders(Map<String, List<String>> headers) {
        if (headers == null)
            responseHeaders = null;
        else {
            if (responseHeaders == null)
                responseHeaders = new Headers();
            else
                responseHeaders.clear();
            responseHeaders.putAll(headers);
        }
    }

    @Override
    public void setResponseHeader(String key, String value) {
        setResponseHeader(key, Collections.singletonList(value));
    }

    @Override
    public void setResponseHeader(String key, List<String> value) {
        if (responseHeaders == null)
            responseHeaders = new Headers();

        responseHeaders.put(key, value);
    }

    @Override
    @Property({MessageContext.HTTP_RESPONSE_HEADERS, Packet.OUTBOUND_TRANSPORT_HEADERS})
    public Map<String, List<String>> getResponseHeaders() {
        return responseHeaders;
    }

    @Override
    public void setStatus(int status) {
        this.status = status;
        // Servlet containers don't seem to like setting of the value multiple times
        // Moving the following to getOutput()
        //response.setStatus(status);
    }

    @Override
    @Property(MessageContext.HTTP_RESPONSE_CODE)
    public int getStatus() {
        return status;
    }

    @Override
    public void setContentTypeResponseHeader(@NotNull String value) {
        response.setContentType(value);
    }

    @Override
    public @NotNull
    InputStream getInput() throws IOException {
        return request.getInputStream();
    }

    @Override
    public @NotNull
    OutputStream getOutput() throws IOException {
        response.setStatus(status);
        if (responseHeaders != null)
            for (Map.Entry<String, List<String>> entry : responseHeaders.entrySet()) {
                String name = entry.getKey();
                if (name == null)
                    continue;
                if (name.equalsIgnoreCase("Content-Type") || name.equalsIgnoreCase("Content-Length"))
                    continue;   // ignore headers that interfere with the operation
                for (String value : entry.getValue())
                    response.addHeader(name, value);
            }
        return response.getOutputStream();
    }

    public @NotNull
    WebServiceContextDelegate getWebServiceContextDelegate() {
        return this;
    }

    public Principal getUserPrincipal(Packet p) {
        return request.getUserPrincipal();
    }

    public boolean isUserInRole(Packet p, String role) {
        return request.isUserInRole(role);
    }

    public @NotNull
    String getEPRAddress(Packet p, WSEndpoint endpoint) {
        PortAddressResolver resolver = adapter.owner.createPortAddressResolver(getBaseAddress(), endpoint.getImplementationClass());
        String address = resolver.getAddressFor(endpoint.getServiceName(), endpoint.getPortName().getLocalPart());
        if (address == null)
            throw new WebServiceException(WsservletMessages.SERVLET_NO_ADDRESS_AVAILABLE(endpoint.getPortName()));
        return address;
    }

    public String getWSDLAddress(@NotNull Packet request, @NotNull WSEndpoint endpoint) {
        String eprAddress = getEPRAddress(request, endpoint);
        if (adapter.getEndpoint().getPort() != null)
            return eprAddress + "?wsdl";
        else
            return null;
    }

    @Override
    @Property(MessageContext.HTTP_REQUEST_METHOD)
    public @NotNull
    String getRequestMethod() {
        return request.getMethod();
    }

    @Override
    public boolean isSecure() {
        return request.isSecure();
    }

    @Override
    public Principal getUserPrincipal() {
        return request.getUserPrincipal();
    }

    @Override
    public boolean isUserInRole(String role) {
        return request.isUserInRole(role);
    }

    @Override
    public String getRequestHeader(String headerName) {
        return request.getHeader(headerName);
    }

    @Override
    @Property(MessageContext.QUERY_STRING)
    public String getQueryString() {
        return request.getQueryString();
    }

    @Override
    @Property(MessageContext.PATH_INFO)
    public @NotNull
    String getPathInfo() {
        return request.getPathInfo();
    }

    @Override
    public @NotNull
    String getRequestURI() {
        return request.getRequestURI();
    }

    @Override
    public @NotNull
    String getRequestScheme() {
        return request.getScheme();
    }

    @Override
    public @NotNull
    String getServerName() {
        return request.getServerName();
    }

    @Override
    public @NotNull
    int getServerPort() {
        return request.getServerPort();
    }

    @Override
    public @NotNull
    String getContextPath() {
        return request.getContextPath();
    }

    public @NotNull
    String getBaseAddress() {
        return getBaseAddress(request);
    }

    static @NotNull
    String getBaseAddress(HttpServletRequest request) {
        StrWriter buf = new StrWriter();
        buf.append(request.getScheme());
        buf.append("://");
        buf.append(request.getServerName());
        buf.append(':');
        buf.append(request.getServerPort());
        buf.append(request.getContextPath());

        return buf.toString();
    }

    @Override
    public Object getRequestAttribute(String key) {
        return request.getAttribute(key);
    }

    @Property(MessageContext.SERVLET_CONTEXT)
    public ServletContext getContext() {
        return context;
    }

    @Property(MessageContext.SERVLET_RESPONSE)
    public HttpServletResponse getResponse() {
        return response;
    }

    @Property(MessageContext.SERVLET_REQUEST)
    public HttpServletRequest getRequest() {
        return request;
    }

    @Property(JAXWSProperties.HTTP_REQUEST_URL)
    public String getRequestURL() {
        return request.getRequestURL().toString();
    }

    @Override
    public String getProtocol() {
        return request.getProtocol();
    }

    @Override
    public void setContentLengthResponseHeader(int value) {
        response.setContentLength(value);
    }

    @Override
    public String getCookie(String name) {
        Cookie[] cookies = request.getCookies();
        if (cookies != null)
            for (Cookie cookie : cookies)
                if (cookie.getName().equals(name))
                    return cookie.getValue();
        return null;
    }

    @Override
    public void setCookie(String name, String value) {
        Cookie cookie = new Cookie(name, value);
        response.addCookie(cookie);
    }

    @Property(Packet.HA_INFO)
    public HaInfo getHaInfo() {
        if (haInfo == null) {
            String replicaInstance = null;
            String key = null;
            String jrouteId = null;
            Cookie[] cookies = request.getCookies();
            if (cookies != null) {
                for (Cookie cookie : cookies)
                    if (replicaInstance == null && cookie.getName().equals("JREPLICA"))
                        replicaInstance = cookie.getValue();
                    else if (key == null && cookie.getName().equals("METRO_KEY"))
                        key = cookie.getValue();
                    else if (jrouteId == null && cookie.getName().equals("JROUTE"))
                        jrouteId = cookie.getValue();
                if (replicaInstance != null && key != null) {
                    String proxyJroute = request.getHeader("proxy-jroute");
                    boolean failOver = jrouteId != null && proxyJroute != null && !jrouteId.equals(proxyJroute);
                    haInfo = new HaInfo(key, replicaInstance, failOver);
                }
            }
        }
        return haInfo;
    }

    public void setHaInfo(HaInfo replicaInfo) {
        this.haInfo = replicaInfo;
    }

    protected PropertyMap getPropertyMap() {
        return model;
    }

    private static final PropertyMap model;

    static {
        model = parse(ServletConnectionImpl.class);
    }
}

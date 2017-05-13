package com.fx.webkit.webapp;

import com.utils.console.TConsole;
import com.resources.dict.MimeMappings;
import java.io.*;
import java.net.*;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Handler extends URLStreamHandler {

    public static interface ExceptionHandler {

        public void onException(Throwable e, Object source, String method);
    }

    public final static List<ExceptionHandler> exceptionHandlers = new LinkedList<>();

    /**
     * Rejestruje handler dla danego protokołu. Klasa musi nazywać się "Handler"
     * pakiet musi pokrywać się z nazwą protokołu
     */
    public static void register() {
        if (!"Handler".equals(Handler.class.getSimpleName()))
            throw new RuntimeException("Klasa musi nazywać się 'Handler'");
        String name = Handler.class.getPackage().getName();
        String protocol = name.substring(name.lastIndexOf(".") + 1);
        System.setProperty("java.protocol.handler.pkgs", name.substring(0, name.lastIndexOf(".")));
    }

    public Handler() {

    }

    public Handler(ClassLoader classLoader) {

    }

    @Override
    protected URLConnection openConnection(URL u) throws IOException {
        try {
            return new HConn(u);
        } catch (IOException | URISyntaxException e) {
            exceptionHandlers.stream().forEach((handler) -> {
                handler.onException(e, this, "openConnection");
            });
            TConsole.printErr(e);
            throw new IOException(e);
        }
    }

}

class HConn extends HttpURLConnection {

    InputStream in;
    private final Map<String, String> headers = new HashMap<>();

    public HConn(URL url) throws IOException, URISyntaxException {
        super(url);
        headers.put("content-type", MimeMappings.get(url.getFile()));
    }

    @Override
    public void disconnect() {
        try {
            if (in != null)
                in.close();
        } catch (IOException ex) {
            Handler.exceptionHandlers.stream().forEach((handler) -> {
                handler.onException(ex, this, "disconnect");
            });
        }
    }

    @Override
    public boolean usingProxy() {
        return false;
    }

    @Override
    public void connect() throws IOException {
        try {
            in = getClass().getResourceAsStream(getURL().getPath());
            if (in == null)
                return;

            headers.put("", "HTTP/1.1 200 OK");
            headers.put("content-length", "" + in.available());

            doInput = true;
            connected = true;

            //  conn.connected = true;
            // conn.allowUserInteraction = true;
        } catch (Exception e) {
            Handler.exceptionHandlers.stream().forEach((handler) -> {
                handler.onException(e, this, "connect");
            });
            TConsole.printErr(e);
            throw new IOException(e);
        }
    }

    @Override
    public synchronized InputStream getInputStream() throws IOException {
        return in != null ? in : new ByteArrayInputStream(new byte[0]);
    }

    @Override
    public String getHeaderField(String name) {
        return headers.get(name);
    }

    @Override
    public Map<String, List<String>> getHeaderFields() {
        final Map<String, List<String>> map = new LinkedHashMap<>();
        headers.forEach((String t, String u) -> {
            List<String> list = new LinkedList<>();
            list.add(u);
            map.put(t, list);
        });
        return map;
    }

    @Override
    public int getResponseCode() throws IOException {
        return in != null ? 200 : 404;
    }

    @Override
    public String getResponseMessage() throws IOException {
        return "OK";
    }

}

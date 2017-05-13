package com.servers.http;

import com.exceptions.EError;
import com.mlogger.Log;
import com.sun.net.httpserver.*;
import java.io.*;
import java.net.InetSocketAddress;

public abstract class HttpServer {

    public final HttpContext context;
    public final com.sun.net.httpserver.HttpServer server;

    public HttpServer(InetSocketAddress intf) throws IOException {
        server = com.sun.net.httpserver.HttpServer.create(intf, 0);
        context = server.createContext("/", new MyHandler());
        server.setExecutor(null); // creates a default executor
        server.start();
    }

    public abstract void processRequest(HttpReq request, String path) throws Exception;

    public class MyHandler implements HttpHandler {

        @Override
        public void handle(HttpExchange t) throws IOException {
            long ts = System.currentTimeMillis();
            final HttpReq req = new HttpReq(t);
            String path = t.getRequestURI().getPath();

            Headers hdr = req.getResponseHeaders();

            hdr.set("Pragma", "no-cache");
            hdr.set("Cache-Control", "no-cache, no-store, must-revalidate");
            hdr.set("Expires", "0");

            try {
                processRequest(req, path);
            } catch (Throwable e) {
                Log.error(e);
                if (req.isCommited())
                    return;

                int httpStatus = 500;

                if (e instanceof FileNotFoundException)
                    httpStatus = 404;

                Headers resp = req.getResponseHeaders();
                resp.set("Content-Type", "text/plain");

                StringWriter sw = new StringWriter();
                e.printStackTrace(new PrintWriter(sw));

                byte[] data = sw.toString().getBytes("UTF-8");
                t.sendResponseHeaders(httpStatus, data.length);
                try (OutputStream os = t.getResponseBody()) {
                    os.write(data);
                }

            } finally {
                Log.event("req", t.getRequestURI() + " " + (System.currentTimeMillis() - ts) + "ms");
            }

        }

    }

}

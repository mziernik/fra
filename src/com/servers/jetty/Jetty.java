package com.servers.jetty;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppConfig;
import com.context.AppContext;
import com.events.EventListeners;
import com.intf.runnable.Runnable1;
import com.servers.Connector;
import com.servers.WebAppServer;
import com.servlet.MainServlet;
import com.thread.TThread;
import java.util.concurrent.Executor;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.eclipse.jetty.proxy.ConnectHandler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.websocket.server.WebSocketHandler;
import org.eclipse.jetty.proxy.ProxyServlet;
import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.SecureRequestCustomizer;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.SslConnectionFactory;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.util.component.AbstractLifeCycle;
import org.eclipse.jetty.util.component.LifeCycle;
import org.eclipse.jetty.util.ssl.SslContextFactory;
import org.eclipse.jetty.util.thread.QueuedThreadPool;

public class Jetty extends WebAppServer {

    private final static ThreadGroup group = new ThreadGroup("jetty");

    private final Server server = new Server(new QueuedThreadPool() {
        @Override
        protected Thread newThread(Runnable runnable) {
            return new Thread(group, runnable);
        }
    });

    private final ServletContextHandler context = new ServletContextHandler();
    public final static EventListeners<Runnable1<ServletContextHandler>> onContextCreated = new EventListeners<>();

    public Jetty() throws Exception {

        boolean hasProxy = false;

        for (Connector sc : config.connectors) {
            ServerConnector connector = getConnector(sc);
            hasProxy |= "proxy".equals(connector.getName());
            server.addConnector(connector);
        }

        HandlerCollection handlers = new HandlerCollection();
        server.setHandler(handlers);

        handlers.addHandler(new WebSocketHandler.Simple(JettyWebSocket.class));

        if (hasProxy) {
            ServletContextHandler proxyContext = new ServletContextHandler(handlers, "/");
            proxyContext.addServlet(new ServletHolder(ProxyServlet.class), "/");
            proxyContext.setVirtualHosts(new String[]{"@proxy"});
        }

        ServletContextHandler httpContext = new ServletContextHandler(handlers, "/");
        httpContext.addServlet(new ServletHolder(MainServlet.class), "/");
        httpContext.setVirtualHosts(new String[]{"@http", "@https"});

        httpContext.addLifeCycleListener(new AbstractLifeCycle.AbstractLifeCycleListener() {
            @Override
            public void lifeCycleStarted(LifeCycle event) {

                if (!event.isStarted())
                    return;

                ProxyServlet2.executor = (Executor) httpContext.getAttribute("org.eclipse.jetty.server.Executor");

                com.context.WebAppContext.getInstance().contextInitialized(
                        new ServletContextEvent(httpContext.getServletContext()));

                if (config.autoRunBrowserURL != null && !config.autoRunBrowserURL.isEmpty())
                    Utils.runBrowser(config.autoRunBrowserURL);
            }

        });

        if (hasProxy) {
            // Setup proxy handler to handle CONNECT methods
            ConnectHandler connectHandler = new ConnectHandler() {
                @Override
                protected boolean handleAuthentication(HttpServletRequest request, HttpServletResponse response, String address) {
//            String authorization = request.getHeader(PROXY_AUTHORIZATION);
//            if (authorization == null) {
//                response.setStatus(HttpServletResponse.SC_PROXY_AUTHENTICATION_REQUIRED);
//                response.setHeader(PROXY_AUTHENTICATE, "Basic realm=\"Fake Realm\"");
//                return false;
//            } else if (authorization.equals("Basic am9obmRvZTpwYXNz")) {
//                return true;
//            }
                    //       response.setStatus(HttpServletResponse.SC_PROXY_AUTHENTICATION_REQUIRED);
                    //  response.setHeader(HttpServletResponse., "Basic realm=\"Fake Realm\"");
                    return true;
                }
            };
            handlers.addHandler(connectHandler);
        }

        httpContext.setAttribute("javax.servlet.context.tempdir", AppContext.tempPath.toString());
        for (Runnable1<ServletContextHandler> r : onContextCreated)
            r.run(httpContext);

        WebAppServer.context = httpContext.getServletContext();

        AppConfig.setServerName(httpContext.getServletContext().getServerInfo());

    }

    @Override
    protected void doStart() throws Exception {
        server.start();
    }

    private ServerConnector getConnector(Connector sc) {
        ServerConnector connector = new ServerConnector(server);

        if (sc.isSecure()) {

            HttpConfiguration https = new HttpConfiguration();
            https.addCustomizer(new SecureRequestCustomizer());

            https.setSecureScheme("https");
            https.setSendServerVersion(true);
            https.setSendDateHeader(false);

            SslContextFactory ssl = new SslContextFactory();
            ssl.setKeyStorePath(sc.getKeystoreFile());
            ssl.setKeyStorePassword(sc.getKeystorePass());

            ssl.setExcludeCipherSuites(
                    "SSL_RSA_WITH_DES_CBC_SHA",
                    "SSL_DHE_RSA_WITH_DES_CBC_SHA",
                    "SSL_DHE_DSS_WITH_DES_CBC_SHA",
                    "SSL_RSA_EXPORT_WITH_RC4_40_MD5",
                    "SSL_RSA_EXPORT_WITH_DES40_CBC_SHA",
                    "SSL_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA",
                    "SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA");

            connector = new ServerConnector(server,
                    new SslConnectionFactory(ssl, "http/1.1"),
                    new HttpConnectionFactory(https));
        }

        connector.setPort(sc.getLocalPort());
        connector.setHost(sc.getLocalAddress());
        connector.setName(sc.getScheme());

        sc.serverConnector = connector;
        return connector;

    }
}

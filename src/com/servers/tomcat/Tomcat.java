package com.servers.tomcat;

import com.resources.dict.MimeMappings;
import com.servlet.websocket.MainWebSocketJsr;
import com.utils.Utils;
import com.utils.Is;
import com.context.*;
import com.mlogger.Log;

import static com.context.AppContext.*;

import com.exceptions.CoreException;
import com.servers.WebAppServer;
import com.servlet.MainServlet;
import com.thread.TThread;
import java.io.*;
import java.util.Map.Entry;
import javax.servlet.*;
import javax.websocket.server.*;
import org.apache.catalina.*;
import org.apache.catalina.connector.*;
import org.apache.catalina.core.*;
import org.apache.coyote.http11.Http11NioProtocol;
import org.apache.tomcat.websocket.server.*;

public class Tomcat extends WebAppServer {

    protected StandardContext context;
    public final org.apache.catalina.startup.Tomcat tomcat;
    public final Service service;

    public Tomcat() throws Exception {

        tomcat = new org.apache.catalina.startup.Tomcat();

        System.setProperty("catalina.home", varPath.toString());

        // sciezka bazowa w tempie (tomcat tworzy tam m.in. katalog work)
        tomcat.setBaseDir(tempPath.toString());

        service = tomcat.getService(); //musi być wywołane po ustawieniu catalina.home

        if (config.connectors.isEmpty())
            Log.error("Nie zdefionowano konektorów");

        for (com.servers.Connector sc : config.connectors) {
            TomcatConnector conn = new TomcatConnector(sc);
            service.addConnector(conn);

            if (!sc.isSecure())
                tomcat.setConnector(conn);

        }

        if (!webPath.exists())
            webPath.mkdirs();

        if (context == null)
            context = (StandardContext) tomcat.addContext(WebAppServer.contextPath, webPath.toString());

        AppConfig.setServerName(context.getServletContext().getServerInfo() + " (embedded)");

        WebAppServer.context = context.getServletContext();

        Wrapper servlet = org.apache.catalina.startup.Tomcat.addServlet(context,
                "main", MainServlet.class.getName());

        servlet.setAsyncSupported(true);
        servlet.setLoadOnStartup(1);
        //  context.addServletMapping("/", "main");

        for (Entry<String, String> en : config.mimeTypes.entrySet())
            context.addMimeMapping(en.getKey(), en.getValue());

        for (String[] mime : MimeMappings.mappings)
            if (!config.mimeTypes.containsKey(mime[0]))
                context.addMimeMapping(mime[0], mime[1]);

        // zdarzenia inicjalizacji kontekstu
        if (!Boolean.FALSE.equals(config.initWebSocketServer))
            context.addApplicationListener(
                    WsContextListener.class.getName());

        context.addApplicationListener(TomcatListenner.class.getName());

    }

    @Override
    protected void doStart() throws LifecycleException {
        tomcat.start();
    }

    public static class TomcatListenner implements ServletContextListener {

        public TomcatListenner() {
            super();
        }

        @Override
        public void contextInitialized(final ServletContextEvent sce) {

            final ServletContext ctx = sce.getServletContext();
            try {
                if (AppContext.instance() == null)
                    throw new CoreException("Brak instancji kontekstu");

                try {
                    WsServerContainer con
                            = (WsServerContainer) sce.getServletContext()
                                    .getAttribute(ServerContainer.class.getName());

                    ServerEndpointConfig.Builder endpoint = ServerEndpointConfig.Builder.create(MainWebSocketJsr.class, "/{*}");
                    endpoint.configurator(new MainWebSocketJsr.MainWebSocketConfigurator());
                    con.addEndpoint(endpoint.build());
                } catch (Throwable e) {
                    Log.error(e);
                }

                WebAppContext.getInstance().contextInitialized(sce);

                if (config.autoRunBrowserURL != null && !config.autoRunBrowserURL.isEmpty())
                    Utils.runBrowser(config.autoRunBrowserURL);

            } catch (Throwable ex) {
                AppContext.logger.error("Tomcat", ex);
            }
        }

        @Override
        public void contextDestroyed(ServletContextEvent sce) {
            WebAppContext.getInstance().contextDestroyed(sce);
        }

//        @Override
//        public void sessionCreated(HttpSessionEvent hse) {
//            WebAppContext.getInstance().sessionCreated(hse);
//        }
//
//        @Override
//        public void sessionDestroyed(HttpSessionEvent hse) {
//            WebAppContext.getInstance().sessionDestroyed(hse);
//        }
    }

}

class TomcatConnector extends Connector {

    TomcatConnector(com.servers.Connector conn) throws IOException {
        super(Http11NioProtocol.class.getName());

        conn.serverConnector = this;
        setPort(conn.getLocalPort());
        setProxyPort(conn.getPublicPort());

        setAttribute("address", Utils.coalesce(conn.getLocalAddress(), "127.0.0.1"));
        setScheme(conn.getScheme());
        setSecure(conn.isSecure());

        setURIEncoding("UTF-8");
        setMaxPostSize(Utils.coalesce(conn.getMaxPostSize(), 1024 * 1024 * 100));
        //     setMaxHeaderCount(Utils.coalesce(conn.getMaxHeaderCount(), 1000));
        setMaxParameterCount(Utils.coalesce(conn.getMaxParameterCount(), 1000));

        if (!Is.empty(conn.getKeystoreFile())) {

            File cert = AppContext.varPath.getReal(conn.getKeystoreFile());
            if (!cert.exists())
                throw new FileNotFoundException(cert.getCanonicalPath());

            setAttribute("keystoreFile", cert.getCanonicalPath());
            setAttribute("keystorePass", conn.getKeystorePass());
            setAttribute("sslProtocol", conn.getSslProtocol());
            setAttribute("SSLEnabled", true);
        }
    }

}

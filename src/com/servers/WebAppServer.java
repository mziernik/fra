package com.servers;

import com.context.*;
import com.lang.LServers;
import com.thread.TThread;

/**
 * @author Miłosz Ziernik
 * @date 08 stycznia 2016
 * @encoding UTF-8
 */
public abstract class WebAppServer {

    public static javax.servlet.ServletContext context;

    protected static ServerConfig config = AppConfig.server;
    public static boolean disabled;
    private static WebAppServer instance;
    public static String contextPath = "/";

    private static boolean initCalled;

    public static WebAppServer instance() {
        return instance;
    }

    public static void initialize() throws Exception {

        if (disabled && !initCalled) {
            initCalled = true;
            AppContextInitializer.initialize();
            return;
        }

        if (instance != null || disabled)
            return;

        if (instance == null)
            try {
                Class.forName("org.eclipse.jetty.server.Server");
                instance = new com.servers.jetty.Jetty();
            } catch (ClassNotFoundException ex) {
            }

        if (instance == null)
            try {
                Class.forName("org.apache.catalina.startup.Tomcat");
                instance = new com.servers.tomcat.Tomcat();
            } catch (ClassNotFoundException ex) {
            }

        if (instance == null)
            throw new RuntimeException(LServers.WEBAPP_SERVER_LIB_NOT_FOUND.toString());

        for (Connector conn : AppConfig.server.connectors)
            conn.addStartupInfo();

        instance.start();

    }

    public void start() {
        new TThread("WebAppServer starter") {
            @Override
            protected void run() throws Exception {
                try {
                    doStart();
                } catch (Throwable e) {
                    AppContextInitializer.addInitError(e);
                }
            }
        }.start();

    }

    protected abstract void doStart() throws Exception;
}

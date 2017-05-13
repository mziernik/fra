package com.context;

import com.utils.Utils;
import com.utils.Is;
import com.cron.Cron;
import com.exceptions.CoreException;
import com.net.tcp.TcpServer;
import com.servlet.pooling.PoolsManager;
import com.servlet.webservices.WsInitializer;
import com.thread.TThread;
import java.net.URL;
import java.sql.*;
import java.util.*;
import java.util.logging.Level;
import javax.servlet.*;
import javax.servlet.http.*;

import static com.context.AppContext.*;

import com.servers.Connector;
import com.servers.WebAppServer;

public interface WebAppContext
        extends ServletContextListener, ServletContextAttributeListener {


    /*
     public static ServletContext getServletContext() {
     return MainServlet.getInstance().getServletContext();
     }
     */
    public static WebAppContext getInstance() {
        WebAppContext ctx = (WebAppContext) instance();
        if (ctx == null)
            throw new CoreException("Kontekst nie został zainicjalizowany");
        return ctx;

    }

    @Override
    default void contextInitialized(final ServletContextEvent sce) {

        if (!(this instanceof AppContext))
            throw new UnsupportedOperationException("Interfejs nie implementuje klasy "
                    + AppContext.class.getName());

        if (AppContextInitializer.webContextInitCalled
                || AppContextInitializer.hasInitErrors())
            return;

        if (AppContext.instance() == null)
            throw new CoreException("Kontekst usługi nie został zainicjowany.",
                    "Musi istnieć klasa dziedzicząca po AppContext lub WebAppContext. "
                    + "Klasa ta musi zostać zainicjalizowana "
                    + "przy starcie usługi. Można tego dokonać dodając "
                    + "adnotację @WebListener w przypadku WebAppContext "
                    + "lub bezpośrednio wywołać konstruktor AppContext "
                    + "w przypadku standardowej aplikacji");

        AppContextInitializer.webContextInitCalled = true;
        WebAppServer.context = sce.getServletContext();

        try {

            logger.event("WebAppContext initialized, " + sce.getServletContext().getRealPath(""));
            ServletContext ctx = sce.getServletContext();
            String ctxPath = ctx.getContextPath();
            if (ctxPath != null && !ctxPath.isEmpty() && !ctxPath.equals("/"))
                AppContextInitializer.startupInfo.put("CONTEXT_PATH", ctxPath);

            if (getInstance() == null)
                throw new CoreException("Brak instancji kontekstu");

            if (!AppConfig.isWebApp())
                throw new CoreException("Kontekst nie dziedziczy po WebAppContext");

            if (Is.empty(AppConfig.getServerName()))
                AppConfig.setServerName(ctx.getServerInfo());

            AppConfig.process(AppConfig.ProcessStage.CONTEXT_LOADED);
            AppContextInitializer.initialize();

        } catch (Throwable ex) {
            AppContextInitializer.addInitError(ex);
        }
    }

    @Override
    default void contextDestroyed(ServletContextEvent sce) {

        try {
            logger.info("---> Niszczenie kontekstu" + AppConfig.getServiceName()
                    + ", " + WebAppServer.context.getRealPath("/") + " <---");

            terminated = true;

            Cron.stop();

            for (TThread th : TThread.getAllThreads())
                try {
                    th.interrupt();
                } catch (Exception e) {
                    logger.log(Level.SEVERE, null, e);
                }

            try {
                AppContext.instance().onDestroy();
            } catch (Throwable e) {
                logger.log(Level.SEVERE, null, e);
            }

            TcpServer.onContextDestroyed();
            PoolsManager.onServletContextDestroy();
            WsInitializer.contextDestroyed();

            Enumeration<Driver> drivers = DriverManager.getDrivers();
            while (drivers.hasMoreElements()) {
                Driver driver = drivers.nextElement();
                try {
                    DriverManager.deregisterDriver(driver);
                    logger.log(Level.FINE, "Wyrejestrowuję sterownik JDBC " + driver.getClass().getName());
                } catch (SQLException e) {
                    logger.log(Level.SEVERE, null, e);
                }
            }

            logger.shutdown();

            TThread.interuptAll(3000);

        } catch (Throwable e) {
            logger.log(Level.SEVERE, e.getLocalizedMessage(), e);
        }

    }

    /*
     @Override
     default void sessionCreated(HttpSessionEvent hse) {
     Dev.info("Session Created " + hse.getSession().getId());
     // HttpSessionImpl sc = new HttpSessionImpl(hse.getSession());
     }

     @Override
     default void sessionDestroyed(HttpSessionEvent hse) {
     try {
     Dev.info("Session Destroyed " + hse.getSession().getId());
     /// ToDo NullPointer
     HttpSessionImpl sc = new HttpSessionImpl(hse.getSession());
     BaseSession ses = BaseSession.find(sc.getId());
     if (devMode)
     Log.event("Session", "Destroyed " + ses.getId());

     if (ses != null) {
     ses.onDestroy();
     synchronized (BaseSession.sessions) {
     BaseSession.sessions.remove(ses);
     }
     }
     } catch (Throwable e) {
     Log.warning(e);
     }
     }
     */
    public static String getAbsolutePath(String path, HttpServletRequest request, Boolean https) {
        String s = "";
        String t = "";
        String qry = "";
        path = Utils.coalesce(path, "");

        if (path.contains("?")) {
            qry = path.substring(path.indexOf("?"));
            path = path.substring(0, path.indexOf("?"));
        }

        if (!path.startsWith("/"))
            s = "/";
        s += path.trim();

        int destPort = request.getServerPort();
        String destProtocol = request.getScheme();

        Connector conn = Connector.get(https);
        if (conn != null) {
            destPort = conn.getPublicPort();
            destProtocol = conn.getPublicAddress();
        }

        try {
            t = destPort != 80 && destPort != 443
                    ? new URL(destProtocol,
                            request.getServerName(),
                            destPort,
                            Utils.coalesce(request.getContextPath(), "")).toString()
                    : new URL(destProtocol,
                            request.getServerName(),
                            Utils.coalesce(request.getContextPath(), "")).toString();

        } catch (Exception ex) {
        }

        return t + s + qry;
    }

    @Override
    default void attributeAdded(ServletContextAttributeEvent scae) {

    }

    @Override
    default void attributeRemoved(ServletContextAttributeEvent scae) {

    }

    @Override
    default void attributeReplaced(ServletContextAttributeEvent scae) {

    }

}

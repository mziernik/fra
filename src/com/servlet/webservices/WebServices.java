package com.servlet.webservices;

import com.servlet.webservices.jax.*;
import com.context.AppContext;
import com.context.index.Index;
import com.lang.LServlet;
import com.mlogger.Log;
import com.servlet.MainServlet;
import com.sun.xml.ws.api.WSBinding;
import com.sun.xml.ws.api.message.Packet;
import com.sun.xml.ws.api.server.InstanceResolver;
import com.sun.xml.ws.transport.http.DeploymentDescriptorParser;
import com.sun.xml.ws.util.HandlerAnnotationInfo;
import com.sun.xml.ws.util.HandlerAnnotationProcessor;
import com.utils.*;
import com.xml.*;
import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import javax.servlet.*;
import java.util.*;
import javax.servlet.http.*;
import javax.xml.namespace.QName;
import javax.xml.ws.handler.Handler;

public final class WebServices {

    public final static List<WS> websSrvices = new LinkedList<>();
    private static boolean initialized;

    public static class WS {

        final String xml;
        final Set<String> urls = new LinkedHashSet<>();
        final WSServletDelegate delegate;
        private final List<ServletAdapter> adapters = new LinkedList<>();

        public WS(String xml, WSServletDelegate delegate) {
            this.xml = xml;
            this.delegate = delegate;
        }

    }

    private final static JAXWSRIDeploymentProbeProvider probe = new JAXWSRIDeploymentProbeProvider();
    private static ServletContext context;

    private static Throwable initError;

    static final Map<String, WS> urls = new LinkedHashMap<>();

    static void initializeWebServices() throws Exception {
        try {
            if (Index.webServices.isEmpty())
                return;

            Log.info("Inicjalizacja modułu JAX-WS");

            // dodaj wstepnie adresy url do listy aby mozna bylo stwierdzi,
            // czy zadanie sevletu dotyczy webservice zanim sie jeszcze calosc zainicjalizuje
            for (Index.IdxWebService wsEntry : Index.webServices)
                urls.put(new Str(wsEntry.aIWS.urlMapping())
                        .removePrefix("/")
                        .toString(), null);

            for (Index.IdxWebService wsEntry : Index.webServices) {

                if (AppContext.devMode && wsEntry.aIWS.disabledIndevMode())
                    continue;

                XML xml = new XML("<endpoints/>");
                xml.options.header = null;
                xml.attr("version", "2.0");
                xml.attr("xmlns", "http://java.sun.com/xml/ns/jax-ws/ri/runtime");

                XmlNode node = xml.addNode("endpoint");
                node.attr("implementation", wsEntry.clazz.getName());
                node.attr("name", wsEntry.url);
                node.attr("url-pattern", new Str(wsEntry.aIWS.urlMapping())
                        .prefix("/").toString());

                context = MainServlet.getInstance().getServletContext();

                WS ws = parseAdaptersAndCreateDelegate(xml.toString(), wsEntry.aIWS);
                websSrvices.add(ws);

                urls.put(new Str(wsEntry.aIWS.urlMapping())
                        .removePrefix("/")
                        .toString(), ws);

                /* for (ServletAdapter adt : ws.adapters) {

                 WSBinding binding = adt.getEndpoint().getBinding();
                 List<Handler> handlers = new LinkedList<>();
                 for (Class<? extends SOAPHandler> ch : chains) {
                 SOAPHandler chain = ch.newInstance();
                 handlers.add(chain);
                 }

                 binding.setHandlerChain(handlers);
                 }
                 */
                for (ServletAdapter adapter : ws.adapters)
                    probe.deploy(adapter);

                Log.info("Dodano moduł " + wsEntry.aWS.serviceName()
                        + " -> " + wsEntry.aIWS.urlMapping());

            }

        } catch (Throwable e) {
            initError = e;
            throw e;
        } finally {
            initialized = true;
        }
    }

    public static boolean processRequest(String servletPath, HttpServletRequest request, HttpServletResponse response) throws ServletException {

        if (!WsInitializer.hasLibrary)
            return false;

        if ((initialized && websSrvices.isEmpty()))
            return false;

        boolean has = false;

        String first = servletPath.split("/")[0];

        for (String url : urls.keySet())
            has |= url.equals(first);

        if (!has)
            return false;

        if (!initialized)
            new WaitFor(LServlet.INIT_JAX_WS_MODULE.toString(),
                    () -> initialized);

        WS ws = urls.get(first);

        switch (request.getMethod().toLowerCase()) {
            case "get":
                ws.delegate.doGet(request, response, context);
                return true;
            case "post":
                ws.delegate.doPost(request, response, context);
                return true;
            case "head":
                ws.delegate.doHead(request, response, context);
                return true;
            case "put":
                ws.delegate.doPut(request, response, context);
                return true;
            case "delete":
                ws.delegate.doDelete(request, response, context);
                return true;
        }

        return true;

    }

    static void contextDestroyed() {

        for (WS ws : websSrvices) {

            ws.delegate.destroy();

            for (ServletAdapter a : ws.adapters) {
                try {
                    a.getEndpoint().dispose();
                } catch (Throwable e) {
                    Log.warning(e);
                }

                // Emit undeployment probe event for each endpoint
                probe.undeploy(a);
            }

        }

    }

    static WS parseAdaptersAndCreateDelegate(String xml, IWebService intf)
            throws MalformedURLException, InstantiationException, IllegalAccessException {

        final List<Handler> handlers = new LinkedList<>();
        handlers.add(new PreHandler());
        handlers.add(intf.handler().newInstance());

        Class<? extends WsMethodInvoker> cInvoker = intf.invoker();

        final WsMethodInvoker invoker = cInvoker != WsMethodInvoker.class
                ? cInvoker.newInstance() : null;

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        if (classLoader == null)
            classLoader = context.getClass().getClassLoader();

        DeploymentDescriptorParser<ServletAdapter> parser = new DeploymentDescriptorParser<>(
                classLoader, new ServletResourceLoader(context),
                new ServletContainer(context),
                new ServletAdapterList(context));

        List<ServletAdapter> adapters = new LinkedList<>();

        if (invoker != null)
            com.sun.xml.ws.api.server.InstanceResolver.callback
                    = new InstanceResolver.InstanceResolverCallback() {

                @Override
                public Object invoke(Object obj, Packet packet, Method method, Object... os)
                        throws InvocationTargetException, IllegalAccessException {
                    return invoker.invoke(obj, packet, method, os);
                }
            };

        com.sun.xml.ws.util.HandlerAnnotationProcessor.callback
                = new HandlerAnnotationProcessor.HandlerAnnotationProcessorCallback() {

            @Override
            public HandlerAnnotationInfo buildHandlerInfo(Class<?> type, QName qname, QName qname1, WSBinding wsb) {
                HandlerAnnotationInfo info = new HandlerAnnotationInfo();
                info.setHandlers(handlers);
                return info;
            }
        };

        adapters.addAll(parser.parse("", new ByteArrayInputStream(xml.getBytes())));
        /*
         for (ServletAdapter ad : adapters) {
         WSBinding binding = ad.getEndpoint().getBinding();
         binding.setHandlerChain(chain);

         }
         */
        WS ws = new WS(xml, new WSServletDelegate(adapters, context));
        ws.adapters.addAll(adapters);

        return ws;

    }

}

package com.servlet.controller;

import com.utils.reflections.Reflections;
import com.context.index.Index;
import com.exceptions.CoreException;
import com.lang.LServlet;
import com.servlet.Handlers;
import com.servlet.interfaces.*;
import com.servlet.interfaces.Arg.ArgMeta;
import com.servlet.interfaces.JsMethod.JsMethodImpl;
import com.servlet.page_old.Service;
import com.servlet.requests.HttpRequest;
import com.servlet.views.ViewController;
import com.utils.Url;
import com.utils.collections.Params;
import com.utils.reflections.TMethod;
import java.lang.reflect.*;
import java.util.*;
import javax.servlet.http.HttpServlet;
import com.utils.reflections.TClass;

/**
 * Miłosz Ziernik 2013/11/02
 */
public class ControllerMetaData {

    public final Class<? extends Controller> controller;
    public final Controller controllerInstance;
    public final LinkedList<ControllerMetaData> linkedControllers = new LinkedList<>();
    final Endpoint endpoint;
    @Deprecated // -> mappings
    public final LinkedList<String> urls = new LinkedList<>();
    public final HashMap<TMethod, Endpoint> endpointMethods = new HashMap<>();
    public final Set<TMethod> lambdaMethods = new HashSet<>();

    public final boolean internal;
    public final String hashId;
    public final String classId;
    public final boolean isAnonymous;
    public final boolean isAbstract;
    public final boolean hasEndpoint;
    public final LinkedList<JsMethod.JsMethodImpl> jsMethods = new LinkedList<>();
    public final Map<Field, ArgMeta> arguments = new LinkedHashMap<>();
    public final Map<String, EndpointMapping> mappings = new LinkedHashMap<>();

    public ControllerMetaData(HttpServlet servlet, boolean internal) {
        this.internal = internal;
        this.controller = ServletController.class;
        this.controllerInstance = new ServletController(servlet);
        Index.controllers.add(this);
        endpoint = getClass().getDeclaredAnnotation(Endpoint.class);
        hasEndpoint = endpoint != null;
        hashId = "s." + Reflections.getClassHash(servlet.getClass());
        classId = servlet.getClass().getName();
        isAnonymous = servlet.getClass().isAnonymousClass();
        isAbstract = Modifier.isAbstract(servlet.getClass().getModifiers());
    }

    public ControllerEndpoint<?> endpoint() {
        return new ControllerEndpoint(this, null);
    }

    public ControllerEndpoint<?> endpoint(HttpRequest http) {
        return new ControllerEndpoint(this, http);
    }

    public ControllerMetaData(Class<? extends Controller> controller, boolean internal) {
        this.internal = internal;
        this.controller = controller;
        this.controllerInstance = null;

        classId = controller.getCanonicalName();
        hashId = Reflections.getClassHash(controller);

        isAnonymous = controller.isAnonymousClass();
        isAbstract = Modifier.isAbstract(controller.getModifiers());

        if (!isAnonymous) {
            Class<?> enclosingClass = controller.getEnclosingClass();
            if (enclosingClass != null && !Modifier.isStatic(controller.getModifiers()))
                throw new CoreException(LServlet.CLASS_MUST_BE_STATIC.toString(controller.getName()));

        }

        for (Field f : controller.getDeclaredFields()) {
            Arg arg = f.getDeclaredAnnotation(Arg.class);
            if (arg != null)
                arguments.put(f, new ArgMeta(f, f.getType(), arg));
        }

        Endpoint endpoint = controller.getDeclaredAnnotation(Endpoint.class);
        hasEndpoint = endpoint != null;

        // dla klas anonimowych przpisz adnotację rodzica
        if (endpoint == null) {
            Class<?> eCls = controller.getEnclosingClass();
            if (eCls != null && new TClass(eCls).instanceOf(Page.class)) {
                for (ControllerMetaData h : Index.controllers)
                    if (h.controller == eCls) {
                        endpoint = h.controller.getDeclaredAnnotation(Endpoint.class);
                        break;
                    }
                if (endpoint == null)
                    throw new CoreException(LServlet.MASTER_CONTROLLER_DEF_NOT_FOUND.toString(controller.getName()));

            }
        }

        Index.controllers.add(this);

        if (endpoint == null)
            endpoint = Service.EmptyHandler.class.getDeclaredAnnotation(Endpoint.class); // zaslepka

        this.endpoint = endpoint;

        try {

//            if (hasEndpoint
//                    && controller != EmptyHandler.class
//                    && controller != TempRequestPage.class
//                    && (endpoint.title() == null || endpoint.title().trim().isEmpty()))
//                throw new CoreException("Atrybut title adnotacji @Endpoint klasy "
//                        + controller.getName() + " jest pusty");
            Handlers.requests.getInstance().addPage(controller, internal);
            if (hasEndpoint)
                urls.addAll(Arrays.asList(endpoint.url()));

//            if (pc.ipage.classHashName())
//                pc.names.add(Hashes.md5(pc.cpage.getName()).toLowerCase());
            for (String s : urls)
                new EndpointMapping(this, s);

            new EndpointMapping(this, hashId);

            Method[] methods = controller.getDeclaredMethods();
            for (Method m : methods) {

                JsMethod jsMethod = m.getAnnotation(JsMethod.class);

                if (jsMethod != null) {
                    Reflections.checkModifiers(m, Modifier.PUBLIC, -Modifier.ABSTRACT);

                    if (!Void.TYPE.equals(m.getReturnType()))
                        throw new CoreException(LServlet.MEDODA_MUST_RETURN_VOID.toString(controller.getName(), m.getName()));

                    JsMethodImpl impl = new JsMethodImpl(jsMethod, m);
                    jsMethods.add(impl);
                }

                Endpoint aPage = m.getAnnotation(Endpoint.class);
                if (aPage != null) {
                    Reflections.checkModifiers(m, Modifier.PUBLIC, -Modifier.ABSTRACT);

                    TMethod method = new TMethod(m);

                    for (ArgMeta am : method.arguments)
                        if (am.ann == null)
                            throw new CoreException(LServlet.ARG_MUST_HAVE_ANNOTATION.toString(am.name, method));

                    this.endpointMethods.put(method, aPage);
                }
            }
        } catch (Throwable e) {
            throw new CoreException(e).details(LServlet.CLASS.toString(), controller.getName());
        }
    }

    @Override
    public String toString() {
        return (endpoint != null && !endpoint.title().isEmpty()
                ? endpoint.title() + ": " : "") + controller.getName();
    }

    public static ControllerMetaData getByClass(Class<? extends Controller> page) {
        /*
         Endpoint annotation = page.getDeclaredAnnotation(Endpoint.class);

         if (annotation == null){
         Class<? > superclass = page.getSuperclass();
         if (Reflections.classExtends(superclass, Page.class))
         return getByClass((Class<? extends RequestEndpoint>) superclass);


         }
         */
        for (ControllerMetaData h : Index.controllers)
            if (page == h.controller)
                return h;
        return null;
    }

    public static Url getUrl(Controller controller) {
        if (controller == null)
            return null;

        String name = controller.getClass().getName();
        if (name.contains("$$Lambda$"))
            return new Url("/" + ControllerMetaData.getHash(controller));

        return getUrl(controller.getClass());

    }

    public static Url getUrl(Class<? extends Controller> controller) {
        Endpoint ipage = controller.getAnnotation(Endpoint.class);

        if (ipage != null)
            for (String s : ipage.url())
                if (!s.isEmpty() && !s.equals("/"))
                    return new Url("/" + s);

        if (ipage != null)
            for (String s : ipage.url())
                if (!s.isEmpty())
                    return new Url("/" + s);

        return new Url("/" + getHash(controller));
    }

    public static String getHash(Controller controller) {
        if (controller == null)
            return null;

        String name = controller.getClass().getName();
        if (name.contains("$$Lambda$")) {

            String hash = Reflections.getClassHash(controller.getClass());

            if (!EndpointMapping.directMapping.containsKey(hash))
                new EndpointMapping(hash, controller);

            return hash;
        }
        return getHash(controller.getClass());
    }

    public TMethod getEndpointMethod(HttpRequest http) {

        if (endpointMethods.isEmpty()
                || (!http.relativePath.contains("/")
                && http.getQueryString() == null))
            return null;

        String s1 = http.relativePath.contains("/")
                ? http.relativePath.substring(http.relativePath.indexOf("/") + 1)
                : null;

        if (s1 != null && s1.contains("/"))
            s1 = s1.substring(s1.lastIndexOf("/") + 1);

        Params.Param first = http.queryParams.first();
        String s2 = first != null ? first.name : null;

        for (Map.Entry<TMethod, Endpoint> en : endpointMethods.entrySet()) {
            Endpoint mm = en.getValue();

            TMethod em = en.getKey();

            if (mm.url().length == 0
                    && (em.getName().equals(s1)
                    || em.getName().equals(s2)))
                return em;

            if (mm.url().length >= 0)
                for (String s : mm.url())
                    if (s.equals(s1) || s.equals(s2))
                        return em;

        }

        return null;
    }

    public static String getHash(Class<? extends Controller> controller) {
        return Reflections.getClassHash(controller);
    }

    /**
     * Tworzenie zależności pomiędzy kontrolerami
     */
    public static void onControllersLoaded() {

        for (ControllerMetaData cd : Index.controllers)

            if (cd.endpoint.view() != ViewController.class)

                for (ControllerMetaData cd2 : Index.controllers)
                    if (cd2.controller == cd.endpoint.view()) {
                        cd.linkedControllers.add(cd2);
                        break;
                    }
        /*
                 if (page == null)
                 throw new CoreException("Nie znaleziono kontrolera widoku " + cd.controller.getName());


                 // skopiouj deklaracje metod javascript widoku do strony html
                 for (Entry<String, JsMethod.JsMethodImpl> en : cd.jsMethods.entrySet())
                 page.jsMethods.put(en.getKey(), en.getValue());

         */
    }

}

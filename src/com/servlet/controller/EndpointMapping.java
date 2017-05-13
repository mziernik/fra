package com.servlet.controller;

import com.utils.StrUtils;
import com.exceptions.CoreException;
import com.exceptions.ThrowableException;
import com.lang.LServlet;
import com.servlet.requests.HttpRequest;
import com.utils.Str;
import com.utils.reflections.TClass;
import com.utils.reflections.TMethod;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Klasa reprezentuje pojedyncze mapowanie url-a i kontrolera.
 */
public class EndpointMapping {

    public final static Map<String, EndpointMapping> directMapping = new LinkedHashMap<>();
    public final static Map<String, EndpointMapping> patternMapping = new LinkedHashMap<>();

    public final ControllerMetaData meta;
    public final String url;
    private final Pattern pattern;

    public final TMethod method;
    public final boolean staticMethod;
    public final boolean lambda;
    public final Controller lambdaController;

    // konstruktor lambdy
    public EndpointMapping(String hash, Controller controller) {
        lambda = true;
        pattern = null;
        Method mth = null;
        boolean isStatic = true;

        String name = controller.getClass().getCanonicalName();
        if (!name.contains("$$Lambda$"))
            throw new CoreException(LServlet.INCORRECT_CALL.toString());

        Class<? extends Controller> ctrl;
        try {
            ctrl = (Class<? extends Controller>) Class.forName(name.substring(0, name.indexOf("$$Lambda$")));
        } catch (ClassNotFoundException ex) {
            throw new ThrowableException(ex);
        }

        for (Method m : controller.getClass().getDeclaredMethods())
            switch (m.getName()) {
                case "onRequest": {
                    Class<?>[] pTypes = m.getParameterTypes();
                    if (pTypes.length != 1 || !(pTypes[0] == HttpRequest.class))
                        throw new CoreException(LServlet.INVALID_ON_REQUEST_ARG.toString());
                    mth = m;
                    break;
                }
                case "get$Lambda": {
                    Class<?>[] pTypes = m.getParameterTypes();
                    if (pTypes.length != 1 || !(new TClass(pTypes[0]).instanceOf(Controller.class)))
                        throw new CoreException(LServlet.INVALID_GET_LAMBDA_ARG.toString());
                    ctrl = (Class<? extends Controller>) pTypes[0];
                    isStatic = false;
                    break;
                }
            }

        ControllerMetaData meta = ControllerMetaData.getByClass(ctrl);
        if (meta == null)
            throw new CoreException(LServlet.CONTROLLER_DEF_NOT_FOUND.toString(ctrl));

        this.url = hash;
        this.method = new TMethod(mth);
        this.meta = meta;
        this.lambdaController = controller;
        this.staticMethod = isStatic;

        directMapping.put(url, this);
        meta.mappings.put(url, this);
    }

    public static LinkedList<EndpointMapping> get(String url) {
        LinkedList<EndpointMapping> result = new LinkedList<>();

        EndpointMapping ctrl = directMapping.get(url);
        if (ctrl != null)
            result.add(ctrl);

        for (EndpointMapping cm : patternMapping.values())
            if (cm.matches(url))
                result.add(cm);

        return result;
    }

    public EndpointMapping(ControllerMetaData meta, String name) {
        lambda = false;
        method = null;
        staticMethod = false;
        lambdaController = null;
        url = new Str(name).removePrefix("/").toString();
        this.meta = meta;
        {

            String mask = url.toLowerCase();
            String special = "\\[]^$.|+-(){}";

            for (char c : special.toCharArray())
                mask = mask.replace("" + c, "\\" + c);

            mask = mask.replace("?", ".").replace("*", ".*");

            pattern = mask.contains("*") || mask.contains("?") ? Pattern.compile(mask) : null;
        }

        String escape = StrUtils.encodeURIComponent(url);

        if (url.contains("?") || url.contains("&") || url.contains("#"))
            throw new CoreException("Nazwa \"" + name + "\" zawiera niedozwolone znaki");

        if (!escape.equals(url.replace("$", "%24").replace("/", "%2F")))
            throw new CoreException("Nieprawidłowa nazwa \"" + name + "\"", url + " <-> " + escape);

        Map<String, EndpointMapping> dst = pattern != null ? patternMapping : directMapping;

        EndpointMapping target = dst.get(url);

        if (target != null && target.meta != meta)
            throw new CoreException(LServlet.ENDPOINT_NAME_DUPLICATED.toString()
                    + " \""
                    + name + "\" (" + target.meta.controller.getName()
                    + ", " + meta.controller.getName() + ")");

        dst.put(url, this);
        meta.mappings.put(url, this);
    }

    public boolean matches(String url) {
        return pattern != null
                ? pattern.matcher(url).matches()
                : this.url.equals(url);

    }

}

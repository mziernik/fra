package com.webapi.core;

import com.context.AppContext;
import com.context.index.Index;
import com.context.intf.ContextInitStage;
import com.context.intf.ContextInitialized;
import com.intf.callable.Callable;
import com.io.IOUtils;
import com.mlogger.Log;
import com.servlet.interfaces.Arg.ArgMeta;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.MapList;
import com.utils.hashes.Hashes;
import com.utils.reflections.*;
import com.utils.text.StrWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.lang.reflect.*;
import java.util.stream.Collectors;

public class WebApiControllerMeta {

    final static MapList<Class<? extends WebApi>, WebApiControllerMeta> map = new MapList<>();

    public final Class<? extends WebApi> clazz;
    public final TReflection item;
    public final WebApiEndpoint endp;
    public final String name;
    public final boolean auth;
    public final Class<? extends WebApi> returnWebApi;
    public final String hash;

    public WebApiControllerMeta(TReflection method) {
        this.item = method;
        clazz = (Class<? extends WebApi>) method.getDeclaringClass();
        endp = method.getAnnotation(WebApiEndpoint.class);
        method.checkModifiers(-Modifier.ABSTRACT);
        name = !Is.empty(endp.name()) ? endp.name() : method.getName();
        auth = endp.auth();

        Utils.checkId(name, true);

        TClass<?> ret = new TClass<>(method.getReturnType());
        returnWebApi = ret.instanceOf(WebApi.class) ? (Class<? extends WebApi>) ret.raw : null;

        StringBuilder sb = new StringBuilder();
        sb.append(name);
        sb.append(Utils.toString(returnWebApi));

        if (method instanceof TMethod)
            for (ArgMeta a : ((TMethod) method).arguments)
                sb.append(a.name).append(a.cls.getFullName())
                        .append(Boolean.toString(a.required));

        hash = Hashes.idHash6(endp.hash() ^ Hashes.crc32(sb.toString().getBytes()));
    }

    @Override
    public String toString() {
        return clazz.getName() + "." + name;
    }

    public Object invoke(WebApiRequest req, WebApi wapi) throws Exception {
        return invoke(req, wapi, true);
    }

    public Object invoke(WebApiRequest req, WebApi wapi, boolean useControllerInvoker) throws Exception {

        Object[] arr = item instanceof TMethod
                ? ((TMethod) item).createArguments(name -> req.params.getValuesStr(name),
                        req.endpointName,
                        (ArgMeta arg, Object obj) -> arg.cls.raw == WebApiRequest.class ? req : obj)
                : new Object[0];

        if (useControllerInvoker && req.controller.invoker != null)
            return req.controller.invoker.invoke(wapi, req, item, arr);
        else
            return item.invoke(wapi, arr);

    }

    private static String webApiClientFile;
    private static Callable< WebApiController> webApiClientController;

    public static void autoGenerateJsClient(String fileName, Callable< WebApiController> controller) {
        webApiClientFile = fileName;
        webApiClientController = controller;
    }

    @ContextInitialized(stage = ContextInitStage.beforeConfigLoaded,
            ifAvailable = "javax.websocket.CloseReason")
    private static void init() throws Exception {

        for (String s : Index.entries.get("web_api")) {
            TClass<WebApi> cls = new TClass<>(s);

            if (cls.raw.isInterface())
                continue;

            for (TMethod m : cls.getDeclaredMethods(true)
                    .stream()
                    .map((java.lang.reflect.Method m) -> m.getAnnotation(WebApiEndpoint.class) != null
                    ? new TMethod(m) : null)
                    .collect(Collectors.toList()))
                if (m != null)
                    map.add(cls.raw, new WebApiControllerMeta(m));

            for (TField f : cls.getDeclaredFields(true)
                    .stream()
                    .map((TField f) -> f.getAnnotation(WebApiEndpoint.class) != null
                    ? f : null)
                    .collect(Collectors.toList()))
                if (f != null)
                    map.add(cls.raw, new WebApiControllerMeta(f));

        }

        if (!AppContext.devMode
                || webApiClientFile == null
                || webApiClientController == null
                || AppContext.sourcesPath.isEmpty())
            return;

        File file = AppContext.sourcesPath.getFile(webApiClientFile);

        if (!file.exists())
            throw new FileNotFoundException(file.getAbsolutePath());

        StrWriter writer = new StrWriter();
        Html.buildJavascriptClient(webApiClientController.run(), writer);

        String cdata = IOUtils.readUtf(file.toURI().toURL());
        String data = writer.toString();

        if (cdata.equals(data))
            return;
        
        Log.info("Aktualizuję plik " + file.getAbsolutePath());

        IOUtils.write(data, file, Utils.UTF8);
    }

}

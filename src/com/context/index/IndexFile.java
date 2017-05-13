package com.context.index;

import com.utils.Utils;
import com.utils.Is;
import com.utils.reflections.Reflections;
import com.servlet.controller.Controller;
import com.context.AppContext;
import com.context.fra.Framework;
import com.dev.Dev;
import com.context.index.Index.IdxWebService;
import com.context.index.Index.IndexEntry;
import com.exceptions.CoreException;
import com.json.JArray;
import com.json.JObject;
import com.mlogger.Log;
import com.resources.core.IdxRes;
import com.resources.core.ResData;
import com.resources.core.Resources;
import com.servlet.controller.ControllerMetaData;
import com.servlet.handlers.*;
import com.servlet.interfaces.*;
import com.user.RightsScheme;
import com.user.right.UserRight;
import com.servlet.views.ViewController;
import com.servlet.views.ViewControllerMeta;
import com.servlet.websocket.WebSocketController;
import com.servlet.websocket.WebSocketEndpoint;
import com.utils.reflections.TClass;
import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import com.events.Notifier;
import com.events.NotifyListener;
import com.utils.reflections.*;
import java.lang.annotation.Annotation;
import com.context.intf.ContextInitialized;
import com.exceptions.ThrowableException;
import com.servlet.Handlers;
import com.utils.Ready;
import com.utils.collections.TList;
import com.utils.Is;

public class IndexFile implements Iterable<String> {

    public final LinkedHashSet<String> files = new LinkedHashSet<>();
    transient public final String indexFile;
    transient public final String fileList;
    final Boolean isFramework;
    JObject json;

    IndexFile(Boolean isFramework) {
        this.isFramework = isFramework;
        this.indexFile = isFramework == null
                ? null : "META-INF/" + (isFramework ? "fra/" : "") + "index";
        this.fileList = isFramework == null
                ? null : "META-INF/" + (isFramework ? "fra/" : "") + "files";
    }

    public boolean loadIndexClass(Class<?> cls) {
        return true;
    }

    @Override
    public String toString() {
        return indexFile;
    }

    private void addElement(String section, String item) {
        try {
            switch (section) {
                case "controller":
                    new ControllerMetaData(new TClass<Controller>(item).raw, Boolean.TRUE.equals(isFramework));
                    return;

                case "view":

                    Index.views.add(new ViewControllerMeta(new TClass<ViewController>(item).raw));
                    return;

                case "config":
                    Field field = Reflections.getField(item);
                    Index.config.add(field);
                    return;

                case "cron": {
                    new TClass<>(item).invoke(null); //ToDo: przerobic 
                    return;
                }

                case "test": {
                    Class<? extends TestClass> cls = (Class<? extends TestClass>) Class.forName(item);
                    Index.tests.put(cls, cls.getAnnotation(ITestClass.class));
                    return;
                }

                case "websocket": {
                    Class<? extends WebSocketController> cls = (Class<? extends WebSocketController>) Class.forName(item);
                    Index.webSockets.put(cls, cls.getAnnotation(WebSocketEndpoint.class));
                    return;
                }

                case "webservice": {
                    Class<?> cls = Class.forName(item);
                    Index.webServices.add(new IdxWebService(cls));
                    return;
                }

                case "right": {
                    Class<? extends UserRight> cls = (Class<? extends UserRight>) Class.forName(item);
                    Index.rights.add(cls);
                    return;
                }

                case "group": {
                    Class<? extends RightsScheme> cls = (Class<? extends RightsScheme>) Class.forName(item);
                    Index.groups.add(cls);
                    return;
                }

                case "init":
                    TMethod method = new TMethod(item);
                    if (!method.isStatic())
                        throw new CoreException("Metoda " + method + " musi być statyczna");

                    if (method.raw.getParameterCount() > 0)
                        throw new CoreException("Metoda " + method + " nie może przyjmować argumentów");

                    ContextInitialized ann = method.getAnnotation(ContextInitialized.class);

                    if (ann == null)
                        throw new CoreException("Metoda " + method
                                + " nie posiada adnotacji " + ContextInitialized.class.getName());

                    for (String sCls : ann.ifAvailable())
                        try {
                            Class.forName(sCls, false, ClassLoader.getSystemClassLoader());
                        } catch (ClassNotFoundException e) {
                            Log.debug("Pomijam wykonanie metody " + method.getFullName());
                            return;
                        }
                    Index.inits.put(method, ann.stage());
                    return;

                case "resource":
                    Index.resources.add(new IdxRes(item));
                    return;

                case "event":
                    Notifier.register((Class<? extends NotifyListener<? extends Notifier>>) Class.forName(item));
                    return;

            }
        } catch (Throwable e) {
            throw new ThrowableException(e)
                    .details("Item", item)
                    .details("Section", section);
        }
    }

    public void process() throws IOException {

        ResData res = Resources.get(indexFile, false);

        if (res == null)
            return;

        try (InputStream in = res.getInputStream()) {
            json = JObject.parse(in);

            try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                    Resources.getF(fileList, false).getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null)
                    files.add(line);
            }
            for (JArray arr : json.getArrays()) {

                String section = arr.getName();

                for (String s : arr.getValuesStr())
                    try {
                        Index.entries.add(section, s);
                        addElement(section, s);
                    } catch (Throwable e) {
                        if (AppContext.devMode)
                            Log.warning("Index", e);
                        else
                            throw e;
                    }
            }
            boolean hasWS = false;
            boolean hasIWS = false;
            /*
             for (Annotation a : annotations) {
             hasWS |= a.annotationType().getName().equals("javax.jws.WebService");
             hasIWS |= a.annotationType() == IWebService.class;
             }

             if (hasWS || hasIWS) {
             if (!hasWS)
             throw new CoreException("Klasa " + cls.getName()
             + " nie posiada adnotacji javax.jws.WebService");
             if (!hasIWS)
             throw new CoreException("Klasa " + cls.getName()
             + " nie posiada adnotacji " + IWebService.class.getName());
             webServices.add(cls.getName());
             }
             */

            Ready.confirm(IndexFile.class);

        }

    }

    void addClass(TClass clazz) throws Exception {
        Class<?> cls = clazz.raw;

        for (Annotation a : cls.getAnnotations())
            for (IndexEntry id : IndexEntry.all)
                if (id.type == Index.IndexEntryType.ANNOTATION
                        && Is.in(a.annotationType(), id.superClasses))
                    id.addItem(cls.getName());

        final String pckg = cls.getPackage() != null ? cls.getPackage().getName() : "";

        int modifiers = cls.getModifiers();

        for (Method m : cls.getDeclaredMethods())
            for (Annotation a : m.getDeclaredAnnotations())
                for (IndexEntry id : IndexEntry.all)
                    if (id.type == Index.IndexEntryType.ANNOTATION
                            && Is.in(a.annotationType(), id.superClasses))
                        id.addItem(m.getDeclaringClass().getName() + "." + m.getName());

        for (IndexEntry id : IndexEntry.all)
            if (id.type == Index.IndexEntryType.CLASS
                    && !Is.in(cls, id.superClasses)
                    && clazz.instanceOf(id.superClasses))
                id.addItem(cls.getName());

        if (Modifier.isAbstract(modifiers))
            return;

        // ------------------ iteracja pol -------------------------
        for (Field f : cls.getFields()) {

            if (pckg.startsWith("com.config.engine"))
                continue;

            for (Annotation a : f.getAnnotations())
                for (IndexEntry id : IndexEntry.all)
                    if (id.type == Index.IndexEntryType.ANNOTATION
                            && Is.in(a.annotationType(), id.superClasses))
                        id.addItem(f.getDeclaringClass().getName() + "." + f.getName());

            TClass ff = new TClass(f.getType());

            for (IndexEntry id : IndexEntry.all)
                if (id.type == Index.IndexEntryType.FIELD
                        && !Is.in(cls, id.superClasses)
                        && ff.instanceOf(id.superClasses))
                    id.addItem(f.getDeclaringClass().getName() + "." + f.getName());

        }

    }

    /**
     * Wywoływane po przeskanowaniu plików projektu
     *
     * @param second
     */
    boolean compare(IndexFile secondFile) throws IllegalArgumentException, IllegalAccessException {
        if (secondFile == null || secondFile.json == null)
            return false;

        JObject second = secondFile.json;

        boolean result = true;

        for (JArray arr : toJson().getArrays()) {

            TList<String> dst = arr.getValuesStr();
            TList<String> src = second != null
                    ? second.arrayD(arr.getName()).getValuesStr()
                    : new TList<>();

            for (String o1 : dst)
                if (!src.contains(o1)) {
                    Dev.info("Index", "Nowy element: " + o1);
                    try {
                        addElement(arr.getName(), o1);
                    } catch (Throwable e) {
                        Log.error(e);
                    }
                    result = false;
                }

            for (String o1 : src)
                if (!dst.contains(o1)) {
                    Dev.info("Index", "Usunięto element: " + o1);
                    result = false;
                }

        }
        return result;
    }

    JObject toJson() {
        JObject json = new JObject();
        json.options.singleLine(false);

        for (IndexEntry id : IndexEntry.all)
            Is.notNullV(id.toArray(), arr -> json.add(arr.getName(), arr));

        TList<String> fraRes = !Framework.isRunning() && Index.fraIdx.json != null
                ? Index.fraIdx.json.array("resource").getValuesStr()
                : null;

        JArray res = json.arrayC("resource");

        for (IdxRes idxRes : Index.resources)
            if (fraRes == null || !fraRes.contains(idxRes.toString()))
                res.add(idxRes.toString());

        return json;
    }

    @Override
    public Iterator<String> iterator() {
        return files.iterator();
    }

    void addResourceMapping(File classPath, String fName) throws IOException {

        if (fName.endsWith(".java") || fName.endsWith(".class") || fName.endsWith(".rs"))
            return;

        if (fName.startsWith("\\") || fName.startsWith("/"))
            fName = fName.substring(1);

        if (!Handlers.resources.getInstance().canIndex(classPath, fName))
            return;

        String name = fName;
        Index.resources.removeIf(idx -> idx.path.equals(name));
        Index.resources.add(new IdxRes(classPath, fName));
    }
}

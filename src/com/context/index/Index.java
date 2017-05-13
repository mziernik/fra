package com.context.index;

import com.utils.Utils;
import com.utils.Is;
import com.context.unit_test.FraUnitTestContext;
import com.servlet.controller.ControllerMetaData;
import com.context.AppContext;
import com.context.fra.Framework;
import com.context.intf.ContextInitStage;
import com.dev.Dev;
import com.exceptions.CoreException;
import com.exceptions.EError;
import com.resources.core.IdxRes;
import com.servlet.handlers.TestClass;
import com.servlet.interfaces.*;
import com.user.RightsScheme;
import com.user.right.UserRight;
import com.servlet.views.ViewControllerMeta;
import java.lang.reflect.*;
import java.util.*;
import com.servlet.webservices.IWebService;
import com.servlet.websocket.WebSocketController;
import java.io.File;
import com.servlet.websocket.WebSocketEndpoint;
import com.utils.collections.MapSet;
import com.events.NotifyListener;
import com.json.JArray;
import com.mlogger.Log;
import com.servlet.Handlers;
import com.utils.Ready;
import com.utils.collections.MapList;
import com.utils.collections.TList;
import com.utils.reflections.*;
import java.lang.annotation.Annotation;

public class Index {

    public final static HashMap<Class<? extends WebSocketController>, WebSocketEndpoint> webSockets = new HashMap<>();
    public final static LinkedList<IdxWebService> webServices = new LinkedList<>();
    public final static HashMap<Class<? extends TestClass>, ITestClass> tests = new HashMap<>();
    public final static LinkedList<ControllerMetaData> controllers = new LinkedList<>();
    public final static LinkedList<ViewControllerMeta> views = new LinkedList<>();
    public final static LinkedList<Class<? extends UserRight>> rights = new LinkedList<>();
    public final static LinkedList<Class<? extends RightsScheme>> groups = new LinkedList<>();
    public final static Map<TMethod, ContextInitStage> inits = new LinkedHashMap<>();
    public final static LinkedList<IdxRes> resources = new LinkedList<>();
    public final static MapSet<Class<?>, NotifyListener<?>> events = new MapSet<>();
    public final static HashSet<Field> config = new HashSet<>();
    public final static TList<String> allFiles = new TList<>();

    public final static MapList<String, String> entries = new MapList<>();

    public static IndexFile fraIdx = new IndexFile(true);
    public static IndexFile svrIdx = new IndexFile(false);

    @SuppressWarnings("unchecked")
    public static <T> LinkedList<TClass<? extends T>> getEntries(String name, Class<T> cls) {
        LinkedList<TClass<? extends T>> list = new LinkedList<>();
        for (String s : entries.get(name))
            try {
                list.add(new TClass<>(s));
            } catch (Throwable e) {
                Log.warning(e);
            }
        return list;
    }

    //public final LinkedHashSet<String> resourceFiles = new LinkedHashSet<>();
    //----------------------- slowniki ---------------------------
    public static void process() throws Exception {

        if (FraUnitTestContext.isRunning()
                || Framework.isRunning())
            svrIdx = fraIdx;

        final File classPath = AppContext.classPath.getFile();

        try {

            fraIdx.process();
            allFiles.addAll(fraIdx.files);
            if (svrIdx != fraIdx) {
                svrIdx.process();
                allFiles.addAll(svrIdx.files);
            }

            Ready.confirm(Index.class);

            ControllerMetaData.onControllersLoaded();

            Handlers.config.getInstance().initialize();

            Collections.sort(controllers, (ControllerMetaData p1, ControllerMetaData p2) -> {
                String s1 = Utils.coalesce(p1.urls.peek(), "zzzzz");
                String s2 = Utils.coalesce(p2.urls.peek(), "zzzzz");

                if (s1.isEmpty())
                    s1 = "/";
                if (s2.isEmpty())
                    s2 = "/";

                if (s1.startsWith("$"))
                    s1 = "ZZZZZ" + s1;
                if (s2.startsWith("$"))
                    s2 = "ZZZZZ" + s2;

                if (p1.controller.getName().startsWith("com.servlet.pages"))
                    s1 = "ZZZ" + s1;
                if (p2.controller.getName().startsWith("com.servlet.pages"))
                    s2 = "ZZZ" + s2;

                return s1.compareTo(s2);
            });

            if (!AppContext.unitTestMode
                    && AppContext.devMode
                    && Dev.remote == null
                    && classPath.isDirectory())
                new ClassScanner(svrIdx, classPath).start();

        } catch (Exception ex) {
            if (AppContext.unitTestMode || !AppContext.devMode || classPath.isFile())
                throw ex;

            String msg = new ClassScanner(svrIdx, classPath).process();
            EError.addDetails(ex, null, (msg != null ? msg + "\n\n" : "")
                    + "Należy ponownie uruchomić usługę");
            throw ex;
        }

    }

    public static void registerClass(String name, Class<?>... superClasses) {
        new IndexEntry(name, superClasses, IndexEntryType.CLASS);
    }

    public static void registerAnnotation(String name, Class<? extends Annotation>... superClasses) {
        new IndexEntry(name, superClasses, IndexEntryType.ANNOTATION);
    }

    public static void registerField(String name, Class<?>... superClasses) {
        new IndexEntry(name, superClasses, IndexEntryType.FIELD);
    }

    public static class IdxWebService {

        public final javax.jws.WebService aWS;
        public final IWebService aIWS;
        public final Class<?> clazz;
        public final String url;
        public boolean initialized;

        public IdxWebService(Class<?> clazz) {
            this.clazz = clazz;
            aWS = clazz.getAnnotation(javax.jws.WebService.class);
            aIWS = clazz.getAnnotation(IWebService.class);

            if (aWS == null)
                throw new CoreException("Klasa " + clazz.getName()
                        + " nie posiada adnotacji javax.jws.WebService");
            if (aIWS == null)
                throw new CoreException("Klasa " + clazz.getName()
                        + " nie posiada adnotacji " + IWebService.class.getName());

            url = aIWS.urlMapping();
            // addEndpoint((Class<? extends Controller>) clazz, url);
        }

    }

    /*
    public static LinkedHashSet<Field> getFields(Class<?> superClass) throws NoSuchFieldException {
        for (IndexEntry en : IndexEntry.all)
            if (en.type == IndexEntryType.FIELD)
                for (Class<?> cls : en.superClasses)
                    if (cls == superClass) {
                        LinkedHashSet<Field> fields = new LinkedHashSet();
                        for (String s : en.items)
                            fields.add(Reflections.getField(s));
                        return fields;
                    }
        return null;
    }

    public static <T> LinkedHashSet<Class<T>> getClasses(Class<T> superClass)
            throws ClassNotFoundException {
        LinkedHashSet<Class<T>> classes = new LinkedHashSet();
        for (IndexEntry en : IndexEntry.all)
            if (en.type == IndexEntryType.CLASS)
                for (Class<?> cls : en.superClasses)
                    if (cls == superClass)
                        for (String s : en.items)
                            classes.add((Class<T>) Class.forName(s));

        return classes;
    }
     */
    static enum IndexEntryType {
        CLASS,
        FIELD,
        ANNOTATION
    }

    static class IndexEntry {

        public final static List<IndexEntry> all = new LinkedList<>();
        public final String name;
        public final Class<?>[] superClasses;
        public final IndexEntryType type;
        private final TreeSet<String> items = new TreeSet<>();

        private IndexEntry(String name, Class<?>[] superClasses, IndexEntryType type) {
            this.name = name;
            this.superClasses = superClasses;
            this.type = type;
            all.add(this);
        }

        @Override
        public String toString() {
            return name;
        }

        IndexEntry addItem(String item) {
            items.add(item);
            return this;
        }

        public JArray toArray() {
            JArray arr = new JArray(name);
            arr.addAll(items);
            return arr;
        }

    }
}

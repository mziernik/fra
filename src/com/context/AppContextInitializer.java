package com.context;

import com.utils.Utils;
import com.utils.Is;
import com.utils.console.TConsole;
import com.utils.date.TDate;
import com.cache.*;
import com.config.engine.HConfig;
import com.mlogger.ServletLog;
import com.context.index.Index;
import com.context.intf.ContextInitStage;
import com.context.intf.ContextInitialized;
import com.cron.Cron;
import com.exceptions.ServiceException;
import com.lang.LContext;
import com.lang.core.Languages;
import com.mlogger.Log;
import com.mlogger.handlers.ConsoleHandler;
import com.servlet.Handlers;
import com.thread.TThread;
import java.util.*;
import com.user.BaseUsersHandler;
import com.user.right.UserRight;
import com.servlet.webservices.WsInitializer;
import com.utils.Ready;
import com.utils.reflections.TMethod;
import java.util.Map.Entry;

public class AppContextInitializer extends TThread {

    public final static LinkedHashMap<String, String> startupInfo = new LinkedHashMap<>();
    public final static LinkedList<Throwable> initErrors = new LinkedList<>(); // ewentualny blad ladowania kontekstu

    static boolean contextInitCalled = false;
    static boolean webContextInitCalled;

    private AppContextInitializer() {
        super("Context loader");
    }

    public static void initialize() {
        if (contextInitCalled || hasInitErrors())
            return;

        contextInitCalled = true;

        new AppContextInitializer().start();
    }

    @Override
    protected void run() throws Exception {
        if (hasInitErrors())
            return;

        try {

            AppContext.logger.addEventsHandler(new ServletLog());

            CachedData.clearTempFiles();

            Index.process();
            Languages.load();

            doOnInitialize(ContextInitStage.allClassesLoaded);

            UserRight.onIndexLoaded();
            AppContext.fraStatus.load();
            AppContext.serviceStatus.load();

            doOnInitialize(ContextInitStage.beforeConfigLoaded);

            Handlers.config.getInstance().load();
            Ready.confirm(HConfig.class);

            AppContext.configLoaded = true;

            CacheManager.instance.start();

            doOnInitialize(ContextInitStage.afterConfigLoaded);

            WsInitializer.initializeWebServices();

            doOnInitialize(ContextInitStage.beforeUsersLoaded);

            BaseUsersHandler.instance().initialize();

            Cron.initialize();

            doOnInitialize(ContextInitStage.allDone);

        } catch (Throwable e) {
            addInitError(e);
        } finally {
            Ready.confirm(AppContextInitializer.class);
            AppContext.initialized = true;
            Handlers.events.getInstance().displayServiceInfo();
        }

        if (hasInitErrors())
            return;

        Handlers.events.getInstance().onServiceInitialized();

        TConsole.printTs(LContext.APPCONTEXTINIT_CONTEXT_INITIALIZED.toString(AppConfig.getServiceTitle(),
                new TDate().diff(AppContext.startTime)));

    }

    private void doOnInitialize(ContextInitStage stage) throws Exception {
        AppContext.instance().onInitialize(stage);

        Map<TMethod, ContextInitialized> map = new LinkedHashMap<>();
        for (Entry<TMethod, ContextInitStage> en : Index.inits.entrySet())
            if (en.getValue() == stage)
                map.put(en.getKey(), en.getKey().raw.getDeclaredAnnotation(ContextInitialized.class));

        Utils.sortMap(map, (Entry<TMethod, ContextInitialized> o1, Entry<TMethod, ContextInitialized> o2)
                -> o2.getValue().order() - o1.getValue().order());

        for (Entry<TMethod, ContextInitialized> en : map.entrySet()) {
            ContextInitialized ann = en.getValue();
            if (ann.async())
                new ContextInitAsyncThread(en.getKey(), ann).start();
            else
                try {
                    Log.trace("Init", stage.name() + ", " + en.getKey().getFullName());
                    en.getKey().invoke(null);
                } catch (Throwable e) {
                    if (!ann.ignoreErrors())
                        throw new ServiceException(en.getKey().getFullName(), e);
                    Log.error(e);
                }
        }
    }

    private static class ContextInitAsyncThread extends TThread {

        private final TMethod method;
        private final ContextInitialized ann;

        public ContextInitAsyncThread(TMethod method, ContextInitialized ann) {
            super(Is.empty(ann.asynThreadGroupName())
                    ? null : new ThreadGroup(ann.asynThreadGroupName()),
                    "ContextInit: " + method);
            this.method = method;
            this.ann = ann;
        }

        @Override
        public void run() {
            try {
                method.invoke(null);

            } catch (Throwable e) {
                if (ann.ignoreErrors())
                    Log.error(e);
                else
                    AppContextInitializer.addInitError(e);
            }
        }

    }

    public static void addInitError(Throwable ex) {
        initErrors.add(ex);
        AppContext.logger.error(ex);
        if (AppContext.logger.getHandlers(ConsoleHandler.class).isEmpty())
            TConsole.printErr(ex);
    }

    public static boolean hasInitErrors() {
        return !initErrors.isEmpty();
    }

    public static List<Throwable> getInitErrors() {
        return initErrors;
    }

    public static Throwable getInitError() {
        return initErrors.peek();
    }

}

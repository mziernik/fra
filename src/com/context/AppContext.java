package com.context;

import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.dev.Dev;
import com.utils.date.TDate;
import com.mlogger.MLogger;
import com.config.CService;
import com.context.index.*;
import com.context.index.Index.IdxWebService;
import com.context.intf.*;
import com.cron.CronRunnable;
import com.dev.DevAction;
import com.exceptions.CoreException;
import com.exceptions.EError;
import com.fx.FxApplication;
import com.fx.FxContext;
import com.servlet.controller.*;
import com.servlet.handlers.TestClass;
import com.user.RightsScheme;
import com.user.right.UserRight;
import com.servlet.views.ViewController;
import com.servlet.websocket.WebSocketController;
import com.utils.WaitFor;
import java.util.Map.Entry;
import com.events.NotifyListener;
import com.exceptions.*;
import com.lang.core.LangDict;
import com.user.*;
import com.utils.console.TConsole;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.text.StrWriter;
import java.lang.instrument.Instrumentation;
import com.webapi.core.WebApi;
import com.config.engine.interfaces.Cfg;
import com.lang.LContext;
import com.utils.TObject;
import java.util.Locale;

public abstract class AppContext {

    //  public final static List<Exception> initWarnings = new LinkedList<>();
    protected static AppContext instance;
    public final static MLogger logger;
    public final static Instrumentation instrumentation;

    public final static TDate startTime = new TDate();
    //-----------------------------------------------------------------------
    public final static Path tempPath = new Path(); // sciezka temp + nazwa uslugi
    public final static Path logsPath = new Path();
    public final static Path varPath = new Path();
    public final static Path etcPath = new Path();
    public final static Path classPath = new Path(); // wskazuje na katalog plikow .class lub plik jar
    public final static Path webPath = new Path(); //sciezka zasobow (domyslnie katalog web)
    public final static Path sourcesPath = new Path(); // katalog źródeł programu

    //----------------------- flagi --------------------------------
    static boolean initialized = false;
    static boolean terminated = false;
    static boolean configLoaded = false;
    public final static boolean unitTestMode; // tryb testow jednostrkowych
    public final static boolean devMode;

    public final static ServiceStatus fraStatus = new ServiceStatus(true);
    public final static ServiceStatus serviceStatus = new ServiceStatus(false);

    public final static TObject<Locale> locale = new TObject<>();

    static {
        instrumentation = com.context.Agent.instrumentation;
        devMode = !Is.empty(Dev.basedir); // musi być na początku
        TConsole.print("");
        TConsole.printTs("Starting " + Environment.command);

        logger = MLogger.instance();

        unitTestMode = Is.inStackTrace(
                "junit.framework.JUnit4TestAdapter.run",
                "org.apache.tools.ant.taskdefs.optional.junit.JUnitTestRunner.run"
        );

        locale.set(Locale.getDefault());

        if (Environment.hostname.equalsIgnoreCase("localhost"))
            throw new ServiceException("Incorrect hostname: \"" + Environment.hostname + "\"");

        if (devMode)
            logger.addConsoleHandler();

        try {
            Index.registerClass("controller", Controller.class);
            Index.registerClass("view", ViewController.class);
            Index.registerClass("websocket", WebSocketController.class);
            Index.registerClass("webservice", IdxWebService.class);
            Index.registerClass("test", TestClass.class);
            Index.registerClass("right", UserRight.class);
            Index.registerClass("group", RightsScheme.class);
            Index.registerClass("cron", CronRunnable.class);
            // Index.registerField("config", ConfigItem.class, CLink.class);
            Index.registerClass("event", NotifyListener.class);
            Index.registerClass("user_type", UserType.class);
            Index.registerClass("dev_action", DevAction.class);
            Index.registerAnnotation("lang_dict", LangDict.class);

            //  Index.registerClass("web_api_controller", WebApiController.class);
            Index.registerAnnotation("config", Cfg.class);

            Index.registerClass("web_api", WebApi.class);
            Index.registerAnnotation("init", ContextInitialized.class);

            AppConfig.process(AppConfig.ProcessStage.PRE_INIT);
        } catch (Throwable e) {
            AppContextInitializer.addInitError(e);
        }
    }

    public AppContext(String[] args) {
        if (instance != null)
            return;

        try {
            String loggerSourceName = logger.getSourceName();
            instance = this;
            AppConfig.args = args;
            AppConfig.process(AppConfig.ProcessStage.BEFORE_INIT);
            AppConfig.load();
            config();
            AppConfig.process(AppConfig.ProcessStage.AFTER_INIT);

            // jesli nie ustalono ręcznie nazwy źródła
            if ((loggerSourceName == null || loggerSourceName.equals(logger.getSourceName()))
                    && !Is.empty(AppConfig.getServiceTitle()))
                logger.setSourceName(AppConfig.getServiceTitle() + "|" + Environment.hostname);

            if (!AppConfig.isServlet && !Is.empty(AppConfig.instanceLockFile))
                if (!Utils.lockInstance(AppConfig.instanceLockFile)) {
                    System.err.println("");
                    System.err.println(LContext.APPCONTEXT_CANT_RUN_MULTIPLE_INSTANCES.toString());
                    System.err.println("");
                    System.err.println(AppConfig.instanceLockFile);
                    System.exit(1);
                    return;
                }

            if (this instanceof FxContext)
                FxApplication.init(args, (FxContext) this);
            else if (Environment.fxSupported && com.dev.console.DevConsole.enabled)
                com.dev.console.DevConsole.init(args);

            if (AppConfig.isWebApp() && !AppConfig.isServlet)
                com.servers.WebAppServer.initialize();

            if (!AppConfig.isWebApp())
                AppContextInitializer.initialize();

        } catch (Throwable e) {
            if (e instanceof CoreException) {
                CoreException ce = (CoreException) e;

                StrWriter wr = new StrWriter();

                wr.append("\n=========================================="
                        + "========================================================\n");

                wr.append(EError.toString(e, true)).append("\n");

                for (Entry<String, String> en : ce.getDetails().entrySet()) {
                    wr.append("\n");
                    if (en.getKey() != null && !en.getKey().isEmpty())
                        wr.append(en.getKey()).append(": ");
                    wr.append(en.getValue()).append("\n");
                }
                wr.append("\n=========================================="
                        + "========================================================\n");

                System.err.println(wr.toString());
            }

            AppContextInitializer.addInitError(e);

            if (!AppConfig.isWebApp) {
                if (!(e instanceof CoreException))
                    e.printStackTrace();
                System.exit(1);
                return;
            }

            throw new ThrowableException(e);
        }
    }

    /**
     * @return the instance
     */
    public static AppContext instance() {
        if (instance == null)
            throw new RuntimeException(LContext.APPCONTEXT_CONTEXT_NOT_INITIALIZED.toString());
        return instance;
    }

    protected void onInitialize(ContextInitStage stage) throws Exception {

    }

    protected void onDestroy() {

    }

    public static boolean isInitialized() {
        return initialized;
    }

    public static boolean isTerminated() {
        return terminated;
    }

    public static boolean debugMode(Boolean local) {

        if (!AppConfig.inputArgs.contains("-Xdebug"))
            return false;

        if (local == null)
            return true;

        for (String s : AppConfig.inputArgs)
            if (s.startsWith("-Xrunjdwp")
                    && s.contains("transport=dt_shmem")
                    && s.contains("address=javadebug"))
                return true;

        return false;
    }

    public static boolean testMode() {
        return devMode || CService.testMode();
    }

    public static boolean isRelease() {
        return CService.releaseMode();
    }

    public static boolean releaseMode() {
        return CService.releaseMode();
    }

    protected abstract void config() throws Exception;

    public static void waitForConfigLoaded() {
        if (!configLoaded)
            new WaitFor(LContext.APPCONTEXT_READING_CONFIG.toString(),
                    () -> {
                        if (AppContextInitializer.hasInitErrors())
                            throw new ServiceException(AppContextInitializer.getInitError());
                        return initialized || AppContextInitializer.hasInitErrors();
                    });

    }

    public static void waitForContextInitialized() throws RuntimeException {
        waitForContext();
    }

    public static void waitForContext() {
        if (!initialized)
            new WaitFor(LContext.APPCONTEXT_INITIALIZING_CONTEXT.toString(),
                    new Interval(devMode ? 30 : 10, Unit.SECONDS), () -> {
                        if (AppContextInitializer.hasInitErrors()) {
                            Throwable e = AppContextInitializer.getInitError();
                            throw e instanceof RuntimeException
                                    ? (RuntimeException) e
                                    : new ServiceException(e);
                        }

                        return initialized || AppContextInitializer.hasInitErrors();
                    });
    }

}

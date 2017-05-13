package com.context;

import com.context.fra.Framework;
import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.utils.reflections.Reflections;
import com.exceptions.CoreException;
import com.io.IOUtils;

import static com.context.AppContext.*;

import com.context.unit_test.FraUnitTestContext;
import com.dev.Dev;
import com.json.JObject;
import com.json.JValue;
import com.lang.LContext;
import com.resources.core.Resources;
import com.script.ConfFile;
import com.servers.ServerConfig;
import com.utils.collections.Strings;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import com.utils.Is;
import com.utils.console.TConsole;

public class AppConfig {

    public static JObject json;

    public static enum ProcessStage {

        PRE_INIT,
        BEFORE_INIT,
        AFTER_INIT,
        CONTEXT_LOADED
    }

    transient static String[] args = null;
    transient final static Set<String> inputArgs = new LinkedHashSet<>();

    static String serviceName = "";
    static String serviceTitle = "";
    static String serviceLoggerName;
    static String applicationSecureKey;
    static String instanceLockFile;

    public static final ServerConfig server = new ServerConfig();

    //-------------------- flagi -------------
    static boolean isServlet; // tryb serwletu - w połączeniu z webapp, tylko dla serwerów zewnętrznych
    static boolean isWebApp; // zawiera wbudowany serwer HTTP
    static boolean isFx;  // JavaFX
    // --------------------------

    public static Set<String> getInputArgs() {
        return inputArgs;
    }

    public static boolean isFx() {
        return isFx;
    }

    public static boolean isServlet() {
        return isServlet;
    }

    public static boolean isWebApp() {
        return isWebApp;
    }

    public static String getApplicationSecureKey() {
        if (applicationSecureKey == null)
            throw new CoreException(LContext.APPCONFIG_UNDEFINED_KEY.toString());
        return applicationSecureKey;
    }

    static void load() throws Exception {

        String dir = Resources.getMetaInfDir();

        URL url = AppConfig.class.getResource(dir + "config.conf");

        if (url == null)
            throw new FileNotFoundException(dir + "config.conf");

        ConfFile conf = new ConfFile(IOUtils.read(url, Utils.UTF8));
        json = conf.process();
        json.options.quotaNames(false);
        
        TConsole.print(url);

        //  TConsole.print(json);
        Is.notNullV(json.object("service"), (JObject obj) -> {
            serviceName = obj.getStr("name", serviceName);
            serviceTitle = obj.getStr("title", serviceTitle);

            applicationSecureKey = obj.getStr("secureKey", applicationSecureKey);
            instanceLockFile = obj.getStr("instanceLockFile", instanceLockFile);
        });

        Is.notNullV(json.object("server"), (JObject obj) -> {
            server.parse(obj);
        });

        Is.notNullV(json.object("path"), (JObject obj) -> {

            if (obj.has("var"))
                AppContext.varPath.set(new File(obj.getStr("var")).getAbsolutePath());

            if (obj.has("log"))
                AppContext.logsPath.set(new File(obj.getStr("log")).getAbsolutePath());

            if (obj.has("temp"))
                AppContext.tempPath.set(new File(obj.getStr("temp")).getAbsolutePath());

            if (obj.has("etc"))
                AppContext.etcPath.set(new File(obj.getStr("etc")).getAbsolutePath());

            if (obj.has("web"))
                AppContext.webPath.set(new File(obj.getStr("web")).getAbsolutePath());

        });

        for (JValue val : json.objectD("properties").getValues())
            System.setProperty(val.getName(), val.asString());

        Is.notNullV(json.object("logger"), (JObject obj) -> {

            for (JValue val : obj.arrayD("handlers").getValues())
                AppContext.logger.addHandler(val.asString());

            serviceLoggerName = obj.getStr("title", serviceLoggerName);

        });

        if (!Is.empty(serviceLoggerName))
            AppContext.logger.setSourceName(serviceLoggerName);

        // ---------------------- weryfikacja ------------------------
        Utils.checkId(serviceName, true);

        AppContext context = AppContext.instance();

        if (context instanceof FraUnitTestContext) {
            isWebApp = true;
            return;
        }

        Constructor<?>[] constructors = context.getClass().getDeclaredConstructors();

        if (context instanceof WebAppContext) {
            isWebApp = true;

            if (context.getClass().getAnnotation(javax.servlet.annotation.WebListener.class) == null)
                throw new CoreException(LContext.APPCONFIG_LACK_OF_WEBLISTENER_ANNOTATION.toString(context.getClass().getName()));

            boolean hasCorrectConstructor = false;
            for (Constructor<?> c : constructors)
                hasCorrectConstructor |= c.getParameterCount() == 0;
            if (!hasCorrectConstructor)
                throw new CoreException(LContext.APPCONFIG_LACK_OF_CTOR_REQUIRED_IN_SERVLET.toString(context.getClass().getName()));
        } else {
            boolean hasCorrectConstructor = false;
            for (Constructor<?> c : constructors)
                hasCorrectConstructor |= c.getParameterCount() == 1
                        && c.getParameterTypes()[0] == String[].class;
            if (!hasCorrectConstructor)
                throw new CoreException(LContext.APPCONFIG_LACK_OF_CTOR_STR_ARRAY.toString(context.getClass().getName()));
        }

        //na podstawie vieżącje instancji klasy kontekstu (wersja dla jar-a) 
        if (serviceName == null || serviceName.isEmpty())
            throw new CoreException(LContext.APPCONFIG_UNKNOWN_SERVICE_ID.toString(),
                    LContext.APPCONFIG_CALL_SET_SERVICE_NAME.toString(
                            context.getClass().getName()));

        if (serviceTitle == null || serviceTitle.isEmpty())
            throw new CoreException(LContext.APPCONFIG_UNKNOWN_SERVICE_ID.toString(),
                    LContext.APPCONFIG_CALL_SET_SERVICE_TITLE.toString(
                            context.getClass().getName()));

        server.verify();

    }

    static void process(ProcessStage stage) throws Exception {

        switch (stage) {

            case BEFORE_INIT:
                classPath.set(Reflections.getClassPath(AppContext.instance.getClass()));

                if (new Path().getFile("nbproject").exists()
                        || isCorrectSourcesPath(new Path().getFile("src")))
                    throw new CoreException(LContext.APPCONFIG_INVALID_HOME_DIR.toString(new Path()))
                            .details("[NetBeans] Project -> Properties -> Run -> Working Directory");

                if (sourcesPath.isEmpty()) {
                    // spróbuj automatycznie okreslić ścieżkę źródeł na podstawie lokalizacji domowej
                    File file = new Path(new File("").getAbsolutePath()).getParent().getFile("src");
                    if (isCorrectSourcesPath(file))
                        sourcesPath.set(file);
                }
                Dev.sources.add(sourcesPath);
                return;

            case AFTER_INIT:
                varPath.mkdirs();
                webPath.mkdirs();
                tempPath.mkdirs();
                logsPath.mkdirs();
                etcPath.mkdirs();
                return;

        };
        /*
        switch (stage) {
            case PRE_INIT:

                //FixMe: zwraca klasę, główną (moze sie przydac do detekcji wersji embedded)
                // System.getProperty("sun.java.command")
                inputArgs.addAll(ManagementFactory.getRuntimeMXBean().
                        getInputArguments());

                String appd;
                if (Environment.isWindows)
                    appd = System.getenv("APPDATA");
                else {
                    appd = System.getProperty("user.home");
                    if (appd.startsWith("/home/"))
                        appd = new Path(appd, ".local").toString();
                }

                if (Is.empty(appd))
                    appd = Environment.userHome;

                varPath.set(appd);
                tempPath.set(Environment.temp);
                logsPath.set(Environment.temp);
                return;

            case BEFORE_INIT:  // konfiguracja jest już wczytana

                classPath.set(Reflections.getClassPath(AppContext.instance.getClass()));
                webPath.set(System.getProperty("user.dir"));

                if (AppContext.instance instanceof WebAppContext) {
                    isWebApp = true;
                    String name = instance.getClass().getClassLoader().getClass().getName();
                    isServlet = (args == null || args.length == 0)
                            && (name.contains(".jetty.") || name.contains(".catalina."));
                }

                if (!isWebApp && args == null)
                    throw new CoreException("Nieprawidłowe wywołanie konstruktora klasy AppContext");

          

                tempPath.add(serviceName);
                varPath.add(serviceName);
                logsPath.set(varPath, "log");
                if (isWebApp)
                    webPath.set(varPath, "web");

                if (sourcesPath.isEmpty()) {
                    // spróbuj automatycznie okreslić ścieżkę źródeł na podstawie lokalizacji domowej
                    File file = new Path().getParent().getFile("src");
                    if (isCorrectSourcesPath(file))
                        sourcesPath.set(file);
                }

                if (!unitTestMode
                        && !Framework.isRunning()
                        && AppContext.debugMode(true)
                        && !isServlet
                        && !isCorrectSourcesPath(sourcesPath.getFile()))
                    throw new CoreException("Ścieżka plików źródłowych ("
                            + sourcesPath + ") jest nieprawidłowa");
//
//                if (writeableHomeDir) {
//                    etcPath.set(homePath, "etc");
//                    varPath.set(homePath, "var");
//                    tempPath.set(homePath, "tmp");
//                    logsPath.set(homePath, "log");
//                    if (isWebApp)
//                        webPath.set(homePath, "web");
//                    //  resourcesPath.set(homePath, AppConfig.resourcesDirName);
//                }

                if (isServlet)
                    varPath.set(varPath, "var", server.name); // resourcesPath.set(homePath, AppConfig.resourcesDirName);

                return;

            case AFTER_INIT:
                if (!isServlet)
                    System.setProperty("java.io.tmpdir", tempPath.toString());

                if (!isServlet) {
                    etcPath.mkdirs();
                    varPath.mkdirs();
                    if (webPath.isEmpty())
                        webPath.mkdirs();
                    tempPath.mkdirs();
                    logsPath.mkdirs();
                }

                return;

            case CONTEXT_LOADED:
                if (!isWebApp)
                    throw new CoreException("Nieprawidłowy tryb pracy usługi");

                if (AppConfig.isServlet) {
                    //FixMe: Sprawdzic czy Environment.userHome wskazuje na catalina home
                    webPath.set(WebAppServer.context.getRealPath(""));
                    varPath.set(Environment.userHome, "work", serviceName);
                    logsPath.set(Environment.userHome, "logs", serviceName);
                    tempPath.set(Environment.temp, serviceName);
                    varPath.set(Environment.userHome, "appdata", serviceName);
                }

                varPath.mkdirs();
                webPath.mkdirs();
                tempPath.mkdirs();
                logsPath.mkdirs();
        }*/
    }

    private static boolean isCorrectSourcesPath(File file) {
        file = new File(file, AppContext.instance().getClass().getName().replace(".", "/") + ".java");
        return file.exists() && file.isFile();
    }

    public static String getServiceName() {
        return serviceName;
    }

    public static String getServiceTitle() {
        return serviceTitle;
    }

    public static String getAppModeName() {
        Strings name = new Strings();
        if (isFx)
            name.add("FX");

        if (isWebApp)
            name.add("WebApp")
                    .add(isServlet ? "Servlet" : "Embedded");

        if (name.isEmpty())
            name.add("Console");

        return name.toString(" ");
    }

    public static boolean allowMultipleInstances = false;
// pozwol uruchomic wiele kopii aplikacji z tego samego katalogu

    public static boolean rootUserMustExists = true;

    public static String getServerName() {
        return server.name;
    }

    public static void setServerName(String serverName) {
        AppConfig.server.name = serverName;
    }

}

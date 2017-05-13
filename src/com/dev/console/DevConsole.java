package com.dev.console;

import com.config.CHttp;
import com.utils.console.TConsole;
import com.utils.Utils;
import com.context.*;
import com.context.index.Index;
import com.dev.Dev;
import com.dev.DevAction;
import com.fx.FX;
import com.fx.webkit.JsBridge;
import com.fx.webkit.WebKit;
import com.io.IOUtils;
import com.mlogger.Log;
import com.mlogger.console.Console;
import com.mlogger.utils.JsonBuilder;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.ServletInputStreamEx;
import com.sun.javafx.application.LauncherImpl;
import com.thread.RunLater;
import com.thread.TThread;
import com.utils.*;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import com.utils.reflections.TClass;
import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.URL;
import java.util.*;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import netscape.javascript.JSObject;
import org.h2.server.web.WebServer;
import org.h2.tools.Server;

public class DevConsole extends Application {

    public static String idePath;
    public static Integer idePid;
    private static boolean getIdePathCalled = false;

    public static boolean enabled = Environment.isWindows
            && AppContext.devMode
            && !AppContext.unitTestMode;

    public static DevConsole instance = null;

    public final WebKit webKit = new WebKit("webapp:///com/dev/console/index.html", new Bridge());

    private final TList<DevAction> actions = new TList<>();

    public DevConsole() {

        // czekaj na gotowośc webkit-a oraz indeksu
        Ready.on((Throwable error) -> {
            if (error != null)
                return;

            for (TClass<? extends DevAction> cls : Index.getEntries("dev_action", DevAction.class)) {

                DevAction action = cls.newInstance(null);

                if (!action.host.isEmpty()) {
                    boolean allow = false;
                    for (String s : action.host)
                        allow |= Environment.hostname.equalsIgnoreCase(s);
                    if (!allow)
                        continue;
                }

                if (!action.user.isEmpty()) {
                    boolean allow = false;
                    for (String s : action.user)
                        allow |= Environment.userName.equalsIgnoreCase(s);
                    if (!allow)
                        continue;
                }
                actions.add(action);
            }

            Collections.sort(actions, (DevAction o1, DevAction o2) -> Double.compare(o1.order, o2.order));

            RunLater.add(100, () -> {
                FX.sync(() -> {
                    for (DevAction action : actions)
                        webKit.call("addAction", action);
                });
            });

        }, DevConsole.class, Index.class);
    }

    @Override
    public void start(Stage stage) throws IOException, Exception {
        Scene scene = new Scene(webKit.webView, 1100, 600);
        scene.setFill(Color.BLACK);

        stage.setTitle(AppConfig.getServiceTitle() + "::DevConsole");
        stage.setScene(scene);
        stage.show();

        webKit.loadIndexPage();

        stage.setOnCloseRequest((WindowEvent t) -> {
            System.exit(0);
        });

    }

    public static void showSourceFile(String file) {

        try {
            if (file == null
                    || !file.contains("(")
                    || !file.contains(")")
                    || !file.contains(".java"))
                return;

            String[] split = file.split(" ");
            if (split.length != 2)
                return;

            String name = split[0].trim();
            String ff = split[1].trim();

            if (name.contains("."))
                name = name.substring(0, name.lastIndexOf("."));

            if (name.contains("."))
                name = name.substring(0, name.lastIndexOf("."));

            name = name.replace(".", "/") + "/" + ff.substring(1, ff.length() - 1);

            String line = "";

            String fName = name;
            if (fName.contains(":")) {
                line = name.substring(fName.indexOf(":"));
                fName = fName.substring(0, fName.indexOf(":"));
            }
            
            getIdePath();
            if (idePath == null)
                return;

            for (Path p : Dev.sources) {
                File f = p.getFile(fName);
                if (!f.exists())
                    continue;
                Execute.windowsCmd("\"" + idePath + "\" " + "--open " + f.getAbsolutePath() + line);
                return;
            }

        } catch (Throwable e) {
            Log.error(e);
        }
    }

    public class Bridge implements JsBridge {

        @Override
        public void consoleError(Object text) {
            TConsole.printErr(Utils.toString(text));
        }

        @Override
        public void onError(String msg, String file, Integer line, Integer column, JSObject ex) {
            TConsole.printErr(new Strings(msg, file, line).toString(", "));
        }

        public void showSourceFile(String file) {
            DevConsole.showSourceFile(file);
        }

        public void logsReady() {
            RunLater.add(this, 100, (RunLater.Task<Bridge> task, Bridge source) -> {
                FX.sync(() -> {

                    Ready.confirm(DevConsole.class);
                    instance = DevConsole.this;

                    JsonBuilder jb = new JsonBuilder();
                    jb.nameQuota = "\"";

                    jb.obj('[');

                    for (Log log : Console.getLogs())
                        log.toJson(jb, null);
                    jb.obj(']');

                    try {
                        ((JSObject) webKit.getMember("logs"))
                                .call("add", jb.toString());
                    } catch (Throwable e) {
                        TConsole.printErr(e);
                    }
                });

            });

        }

        public void onAction(DevAction action) {
            try {
                action.run();
            } catch (Throwable ex) {
                Log.error(ex);
            }
        }

    }

    public static void init(String[] args) {
        TThread.create("FX Dev Launcher", (TThread thread) -> {
            LauncherImpl.launchApplication(DevConsole.class, args);
        }, (Throwable e) -> {
            AppContextInitializer.addInitError(e);
        }).start();
    }

    public static void getIdePath() throws IOException {
        if (getIdePathCalled)
            return;
        getIdePathCalled = true;

        String name = ManagementFactory.getRuntimeMXBean().getName();
        if (name != null && name.contains("@")) {
            long pid = Long.parseLong(name.substring(0, name.indexOf("@")));

            for (String s : Execute.windowsCmd("wmic Path win32_process Where \"processid="
                    + pid + "\" get parentprocessid")
                    .split("\r\n")) {
                idePid = Utils.strInt(s.trim(), null);
                if (idePid != null)
                    break;
            }

            if (idePid != null)
                for (String s : Execute.windowsCmd("wmic process where \"processid="
                        + idePid + "\" get ExecutablePath")
                        .split("\r\n"))
                    if (s.contains("\\")) {
                        idePath = s.trim();
                        break;
                    }

        }
    }

    public static void handle(HttpRequest http) throws Exception {
        getIdePath();
        if ("showSourceFile".equals(http.params.firstStr()))
            try (ServletInputStreamEx in = http.getInputStream()) {
                DevConsole.showSourceFile(IOUtils.readUtf(in).trim());
            }
    }

}

class ActWww extends DevAction {

    public ActWww() {
        super("$tools");
    }

    @Override
    public void run() throws Exception {
        Utils.runBrowser(CHttp.url(new Url(CHttp.url.value()), "$", null).toString());
    }

}

class ActH2 extends DevAction {

    private static Server server;

    public ActH2() {
        super("H2");
    }

    @Override
    public void run() throws Exception {
        if (server == null) {
            WebServer service = new WebServer();
            server = new Server(service);
            service.setShutdownHandler(server);
            server.start();
        }

        Utils.runBrowser(server.getURL());
    }

}

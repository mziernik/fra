package com.mlogger.console;

import com.utils.console.TConsole;
import com.context.Environment;

import com.fx.FX;
import com.html.js.Call;
import com.json.JObject;
import com.mlogger.Log;
import com.mlogger.LogElement;
import com.mlogger.status.StatusGroup;
import com.mlogger.status.StatusItem;
import com.mlogger.utils.JsonBuilder;
import com.servlet.views.ViewsManager;
import com.utils.Unquoted;
import java.io.Writer;
import java.util.LinkedList;
import netscape.javascript.JSObject;

public class Console {

    private final static LinkedList<Log> logs = new LinkedList<>();

    public static LinkedList<Log> getLogs() {
        synchronized (logs) {
            return new LinkedList<>(logs);
        }
    }

    private static Writer writer;

    public static void addLog(LogElement le) {

        if (!(le instanceof Log) || !CConsole.enabled.value())
            return;

        Log log = (Log) le;

        synchronized (logs) {
            logs.add(log);
            while (logs.size() > CConsole.maxCount.value())
                logs.poll();
        }

        LinkedList<ConsoleView> views = ViewsManager.get(ConsoleView.class);
        if (Environment.fxSupported && views.isEmpty() && com.dev.console.DevConsole.instance == null)
            return;

        JsonBuilder jb = new JsonBuilder();
        jb.nameQuota = "\"";
        log.toJson(jb, null);
        String sLog = jb.toString();

        /*  try {
            if (writer == null)
                writer = new OutputStreamWriter(
                        new BufferedOutputStream(
                                new FileOutputStream(AppContext.logsPath.getFile("data.json"))));

            writer.append(sLog).append(",\n\n");
            writer.flush();
        } catch (Exception ex) {
            TConsole.printErr(ex);
        }*/
        try {
            for (ConsoleView view : ViewsManager.get(ConsoleView.class))
                if (view.ready)
                    view.push(new Call("logs.add", new Unquoted(sLog)));

        } catch (Throwable e) {
            TConsole.printErr(e);
        }

        if (Environment.fxSupported && com.dev.console.DevConsole.instance != null)
            FX.sync(() -> {
                try {
                    //  Timestamp ts = new Timestamp();
                    ((JSObject) com.dev.console.DevConsole.instance.webKit.getMember("logs"))
                            .call("add", sLog);

                    //  ts.consoleDiff("jsWindow.call");
                } catch (Throwable e) {
                    TConsole.printErr(e);
                }
            });

    }

    public static void updateStatus(StatusItem status) {
        if (!CConsole.enabled.value())
            return;

        LinkedList<ConsoleView> views = ViewsManager.get(ConsoleView.class);
        if (Environment.fxSupported && views.isEmpty() && com.dev.console.DevConsole.instance == null)
            return;

        if (StatusGroup.ROOT == null)
            return;

        String sLog = StatusGroup.ROOT.getJson().toString();

        try {
            for (ConsoleView view : ViewsManager.get(ConsoleView.class))
                if (view.ready)
                    view.push(new Call("statuses.add", new Unquoted(sLog)));

        } catch (Throwable e) {
            TConsole.printErr(e);
        }

        if (Environment.fxSupported && com.dev.console.DevConsole.instance != null)
            FX.sync(() -> {
                try {
                    //  Timestamp ts = new Timestamp();
                    ((JSObject) com.dev.console.DevConsole.instance.webKit.getMember("statuses"))
                            .call("add", sLog);

                    //  ts.consoleDiff("jsWindow.call");
                } catch (Throwable e) {
                    TConsole.printErr(e);
                }
            });

    }

}

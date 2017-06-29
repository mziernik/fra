package com.context;

import com.utils.Path;
import com.utils.console.TConsole;
import com.utils.text.StrWriter;
import com.database.QueryRows;
import com.database.queries.*;
import com.database.service.EventsDB;
import com.events.ServiceEvent;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.servlet.requests.HttpRequest;
import com.utils.collections.Strings;
import java.io.File;
import java.lang.management.ManagementFactory;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import static com.context.AppContext.*;
import static com.context.AppContextInitializer.startupInfo;
import com.ui.events.filter.BaseEventsFilterHandler;

public class EventsHandler {

    public void onServiceVersionUpdate(ServiceStatus status) {
        Log.log("Service",
                String.format("Aktualizacja wersji usługi %s.%d (%d) -> %s.%d (%d)",
                        status.prevVersion, status.prevBuild, status.prevRev,
                        status.version, status.build, status.revision));
    }

    public boolean onEvent(Object event) throws Exception {
        return true;
    }

    public void onServiceEvent(ServiceEvent event) throws Exception {

        if (true)
            return;

        EventsDB db = new EventsDB();
        int event_id;

        {
            Insert ins = db.insert("event.events");
            ins.arg("type", event.type().key);
            ins.arg("source", event.source());
            ins.arg("value", event.value());
            if (!event.tags().isEmpty())
                ins.arg("tags", event.tags());
            ins.arg("username", event.username());
            ins.arg("user_id", event.userId());

            HttpRequest http = HttpRequest.getInstance();
            if (http != null) {
                ins.arg("address", http.request.getRemoteAddr());
                ins.arg("url", http.url);
            }

            QueryRows rows = ins.execute();
            event_id = rows.generatedKeys.first().getInt(0);
        }

        MultipleQuery mqry = db.multipleQuery();

        for (ServiceEvent.ServiceEventAttribute attr : event.attributes) {
            Insert ins = mqry.insert(attr.details
                    ? "event.details"
                    : "event.attributes");
            ins.arg("event_id", event_id);
            ins.arg("tags::ARRAY", attr.tags);
            ins.arg("name", attr.displayName);
            ins.arg("value", attr.value);
        }

        for (Entry<String, LinkedList<Integer>> fks : event.getKeys().entrySet()) {
            Insert ins = mqry.insert("event.keys");
            ins.arg("event_id", event_id);
            ins.arg("column_name", fks.getKey());
            ins.arg("keys::ARRAY", fks.getValue());
        }

        mqry.execute();

    }

    public void displayServiceInfo() {

        //  Log.debug("AppConfig", AppConfig.json.toString());
        Log log = new Log(LogKind.INFO);
        log.tag("init");
        log.value(AppConfig.getServiceTitle());

        log.attribute("Paths", "Classes", classPath);
        log.attribute("Paths", "WebApp", webPath);
        log.attribute("Paths", "etc", etcPath);
        log.attribute("Paths", "var", varPath);
        log.attribute("Paths", "Temp", tempPath);

        if (System.getProperty("catalina.home") != null)
            log.attribute("Paths", "CatalinaHome", System.getProperty("catalina.home"));
        //-----------------------------------------------------------------
        StrWriter sb = new StrWriter();
        for (Map.Entry<Object, Object> prop : System.getProperties().entrySet())
            sb.append(prop.getKey()).append(" = ").append(prop.getValue()).append("\n");
        log.data("System properties", sb);
        //-----------------------------------------------------------------
        sb = new StrWriter();
        for (Map.Entry<String, String> prop : System.getenv().entrySet())
            sb.append(prop.getKey()).append(" = ").append(prop.getValue()).append("\n");
        log.data("System environments", sb);

        log.attribute("Command line params", new Strings(AppConfig.args).toString(" "));

        //-----------------------------------------------------------------
        String name = ManagementFactory.getRuntimeMXBean().getName();

        if (name.contains("@"))
            startupInfo.put("PID", name.substring(0, name.indexOf("@")));

        startupInfo.put("HOST", Environment.hostname);
        startupInfo.put("USER", Environment.userName);

        startupInfo.put("VERSION", AppContext.serviceStatus.version
                + "." + AppContext.serviceStatus.build
                + " (" + AppContext.serviceStatus.revision + ") ["
                + AppContext.fraStatus.version + "." + AppContext.fraStatus.build
                + " (" + AppContext.fraStatus.revision + ")]");

        if (AppContext.devMode)
            startupInfo.put("DEV_MODE", "True");

        if (AppConfig.isWebApp())
            startupInfo.put("SERVER", AppConfig.getServerName());

        startupInfo.put("MODE", AppConfig.getAppModeName());
        startupInfo.put("COMMAND", Environment.command);

        startupInfo.put("CLASS_PATH", classPath.toString());
        startupInfo.put("VAR_PATH", varPath.toString());
        startupInfo.put("WEB_PATH", webPath.toString());
        startupInfo.put("TEMP_PATH", tempPath.toString());
        startupInfo.put("LOGS_PATH", logsPath.toString());
        startupInfo.put("CURRENT_PATH", new Path(new File("").getAbsolutePath()).toString());
        if (!sourcesPath.isEmpty())
            startupInfo.put("SOURCES_PATH", sourcesPath.toString());

        startupInfo.put("JRE", Environment.javaVersion);

        ServiceEvent event = new ServiceEvent("Service", "Inicjalizacja")
                .tag("init");

        for (Entry<String, String> en : startupInfo.entrySet())
            event.attribute(en.getKey().toLowerCase(), en.getKey(), en.getValue());

        event.execute();

        TConsole.print("");
        TConsole.printParams("|  ", "  |", AppConfig.getServiceTitle(), startupInfo);

        //-----------------------------------------------------------------
        sb = new StrWriter();
        for (String s : ManagementFactory.getRuntimeMXBean().
                getInputArguments())
            sb.append(s).append("\n");
        log.data("Input arguments", sb);

        logger.addLog(log);
    }

    public void onServiceInitialized() {

    }

    public BaseEventsFilterHandler getFilterHandler() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}

package com.mlogger.storage;

import com.context.AppContext;
import com.io.SearchFiles;
import com.mlogger.Log;
import com.mlogger.LogAttr;
import com.thread.QueueThread;
import com.utils.Path;
import com.utils.collections.SyncList;
import com.utils.console.TConsole;
import com.utils.date.TDate;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedHashSet;

public class LogsStorage extends QueueThread<Log> {

    public final static int MAX_FILE_SIZE = 1024 * 300;
    public final static LinkedHashSet<LogAttr> indexedEntries = new LinkedHashSet<>();
    final static int RAW_PACKET_SIZE = 1024 * 300; // szacowany rozmiar nieskompresowanego pakietu
    final static int WRITE_IDLE_TIME = AppContext.devMode ? 1000 : 5000;
//60000; // czas bezczynności, po kótrym logi 
    // zostana zapisane do pliku nawet jeśli rozmiar pakietu nie osiagnal założonego rozmiaru (60 sekund)
    final static LogsStorage instance = new LogsStorage();
    static LogsFile current;
    static int allLogsCount = 0;
    private boolean initialized;
    public final static SyncList<LogsFile> files = new SyncList<>();

    public static void write(Log log) throws IOException {
        // if (!CLogsStorage.enabled.value())
        //     return;  
        instance.add(log);
    }

    public static void writeAll(Collection<Log> logs) throws IOException {
        // if (!CLogsStorage.enabled.value())
        //     return;
        instance.addAll(logs);
    }

    public LogsStorage() {
        super("Storage Logs Queue");
        setIdleTime(WRITE_IDLE_TIME);
    }

    @Override
    protected void processItem(Log item) throws Exception {

        if (true)
            return;

        if (!initialized) {
            initialized = true;
            for (Path path : new SearchFiles(AppContext.logsPath.toString(), true))
                if (path.endsWith(".mlog"))
                    try {
                        new LogsFile(path.toFile());
                    } catch (Throwable e) {
                        onError(e);
                    }

            files.sort((LogsFile o1, LogsFile o2) -> {
                long t1 = o1.max.getTime() - o1.min.getTime();
                long t2 = o2.max.getTime() - o2.min.getTime();
                return (int) (t1 - t2);

            });

            for (LogsFile lf : files)
                if (lf.isTemporary) {
                    current = lf;
                    break;
                }

        }

        LogsFile lf = getCurrent();
        lf.write(item);

        if (lf.size > MAX_FILE_SIZE) {
            lf.compact();
            createNewFile();
        }

    }

    public static void onError(Throwable e) {
        TConsole.printErr(e);
    }

    @Override
    protected void onException(Throwable e) {
        LogsStorage.onError(e);
    }

    static LogsFile getCurrent() throws IOException {
        if (current == null)
            createNewFile();
        return current;
    }

    static void createNewFile() throws IOException {
        current = new LogsFile(AppContext.logsPath.getFile(new TDate().toString("yyyy-MM-dd HH-mm-ss") + ".mlog"));
    }

    @Override
    protected void onIdle() throws Exception {

    }

}

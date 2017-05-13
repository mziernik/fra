package com.mlogger.storage_old;

import com.mlogger.storage.CLogsStorage;
import com.utils.Path;
import com.utils.console.TConsole;
import com.context.AppConfig;
import com.context.AppContext;
import com.io.*;
import com.mlogger.*;
import com.thread.QueueThread;
import com.utils.collections.TList;
import com.utils.date.TDate;
import java.io.*;
import java.util.*;

@Deprecated
public class LogsStorage extends QueueThread<Log> {

    public final static int MAX_FILE_SIZE = 4 * 1024 * 1024;
    public final static LinkedHashSet<LogAttr> indexedEntries = new LinkedHashSet<>();
    final static int RAW_PACKET_SIZE = 1024 * 300; // szacowany rozmiar nieskompresowanego pakietu
    final static int WRITE_IDLE_TIME = AppContext.devMode ? 1000 : 5000;
//60000; // czas bezczynności, po kótrym logi 
    // zostana zapisane do pliku nawet jeśli rozmiar pakietu nie osiagnal założonego rozmiaru (60 sekund)
    final static LogsStorage instance = new LogsStorage();
    private static LogsFile current;
    public final static List<LogsFile> all = new LinkedList<>();
    static int allLogsCount = 0;

    private static boolean initialized;

    public static int getAllLogsCount() {
        return allLogsCount;
    }

    public static Log read(UUID fileUid, Long logId) throws IOException {
        if (fileUid == null || logId == null || logId < 0)
            return null;

        initialize();

        for (LogsFile logs : all) {
            if (!logs.header.fileUid.equals(fileUid))
                continue;

            if (logId >= logs.header.logsCount)
                return null;

            return logs.read(logId);
        }

        return null;

    }

    //ToDo: Dodać offset
    public static List<LogsFile> readLogs(Long maxDate, LogEntry<?>[] attrs, LogsVisitor intf)
            throws IOException {
        initialize();
        /*
        if (LogsFile.writeAwaiting != null)
            if (maxDate == null || LogsFile.writeAwaiting.minDate <= maxDate) {
                byte[] bpacket = current().currentPacket.getCompressed();
                LogsFile.writeAwaiting.readData(new TInputStream(bpacket),
                        LogsFile.writeAwaiting.logs, maxId, maxDate, attrs, intf);
            }
         */
        List<LogsFile> list = new LinkedList<>();

        for (LogsFile logs : all) {
            if (maxDate != null && logs.header.minDate > maxDate)
                continue;

            LogsFileMeta meta = new LogsFileMeta(logs);

            if (!intf.visit(meta))
                continue;

            list.add(logs);
            if (!logs.read(meta, maxDate, attrs, intf))
                break;
        }

        return list;
    }

    public static void write(Log log) throws IOException {
        if (true)
            return;
        initialize();
        instance.add(log);
    }

    public static void writeAll(Collection<Log> logs) throws IOException {
        if (true)
            return;
        initialize();
        instance.addAll(logs);
    }

    public static void onError(Throwable e) {
        TConsole.printErr(e);
    }

    public static void initialize() throws IOException {

        if (initialized)
            return;

        indexedEntries.add(LogAttr.KIND);
        indexedEntries.add(LogAttr.SOURCE);
        indexedEntries.add(LogAttr.ADDRESS);
        indexedEntries.add(LogAttr.DEVICE);
        indexedEntries.add(LogAttr.USER_AGENT);
        indexedEntries.add(LogAttr.HOST_NAME);
        indexedEntries.add(LogAttr.VERSION);
        indexedEntries.add(LogAttr.USER_NAME);
        indexedEntries.add(LogAttr.TAG);
        indexedEntries.add(LogAttr.SESSION);
        indexedEntries.add(LogAttr.INSTANCE);

        try {

            String currentPhrase = AppConfig.getServiceName()
                    + " " + new TDate().toString("yyyyMMdd-HHmmss");

            for (Path path : new SearchFiles(AppContext.logsPath.toString(), true))
                if (path.endsWith(".mlog"))
                    try {
                        LogsFile logs = new LogsFile(path.toFile());

                        if (current == null && path.toString().contains(currentPhrase))
                            current = logs;

                        allLogsCount += logs.header.logsCount;

                        all.add(logs);
                    } catch (Throwable e) {
                        onError(e);
                    }

            // posortuj, aby uzyskać właściwą kolejność
            Collections.sort(all,
                    (LogsFile o1, LogsFile o2) -> o2.header.created.compareTo(o1.header.created));

            initialized = true;

        } catch (Throwable e) {
            TConsole.printErr(e);
            throw e;
        }
    }

    public static void flush() throws IOException {
        current().flush();
    }

    public static LogsFile current() throws IOException {
        if (current == null)
            createNewLogs();
        return current;
    }

    static void createNewLogs() throws IOException {

        if (current != null)
            current.flush();

        File file = AppContext.logsPath.getFile(
                AppConfig.getServiceName()
                + " " + new TDate().toString("yyyyMMdd-HHmmss") + ".mlog");

        current = new LogsFile(file);

        //   TConsole.printTs("Utworzono plik logów " + current.file.getName());
        synchronized (all) {
            all.add(0, current);
        }

    }

    public static LogsFile getCurrent() throws IOException {
        initialize();
        return current;
    }

    public static void read(UUID fromString, int i) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private LogsStorage() {
        super("Storage Logs Queue");
        setIdleTime(WRITE_IDLE_TIME);
    }

    @Override
    protected boolean canProcess(Log item) {
        return initialized;
    }

    @Override
    protected void onIdle() throws Exception {
        if (current != null)
            current.flush();
    }

    @Override
    protected void onException(Throwable e) {
        LogsStorage.onError(e);
    }

    @Override
    protected void processItem(Log item) throws Exception {

        current(); // utworz, jesli nie istnieje
        TList<Log> items = getQueue();
        items.add(0, item);
        removeAll(items);

        if (current.file.length() > MAX_FILE_SIZE)
            createNewLogs();

        if (!current.header.created.isSameDay(new TDate())) //ZMIANA DATY 
            createNewLogs();

        current.add(items);
    }

}

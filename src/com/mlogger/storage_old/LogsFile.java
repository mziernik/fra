package com.mlogger.storage_old;

import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.io.*;
import com.mlogger.*;
import com.utils.Counter;
import com.utils.collections.TList;
import java.io.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.InflaterInputStream;
import javax.xml.ws.Holder;

@Deprecated
public class LogsFile {

    final File file;
    public final LogsHeader header;
    long fileSize;
    private int lastLogId;
    boolean initialized = false;
    private final AtomicInteger currentReaders = new AtomicInteger();
    private boolean writeLock = false;
    private final static int BUFFER_SIZE = 1024 * 100;

    void beginRead() {
        while (writeLock)
            try {
                Thread.sleep(1);
            } catch (InterruptedException ex) {
                return;
            }
        synchronized (currentReaders) {
            currentReaders.incrementAndGet();
        }
    }

    void endRead() {
        synchronized (currentReaders) {
            currentReaders.decrementAndGet();
        }
    }

    void beginWrite() {
        writeLock = true;
        while (currentReaders.get() > 0)
            try {
                Thread.sleep(1);
            } catch (InterruptedException ex) {
                return;
            }
    }

    void endWrite() {
        writeLock = false;
    }

    public LogsFile(File file) throws IOException {

        fileSize = file.length();
        this.file = file;
        try {
            if (file.length() > 0)

                try (TInputStream in = new TInputStream(
                        new BufferedInputStream(new FileInputStream(file), BUFFER_SIZE))) {
                    header = new LogsHeader(this, in);
                    lastLogId = header.logsCount;
                    // Timestamp ts = new Timestamp();
                    //readFile(in);
                    //   TConsole.printParams("Szczegóły pliku logu", header.toString());
                    // ts.consoleDiff("Czas wczytywania");
                }
            else
                header = new LogsHeader(this, null);

        } finally {
            initialized = true;
        }
    }

    @Override
    public String toString() {
        return header.toString();
    }

    //---------------------------- currentPacket ------------------------------
    final Holder<LogsPacket> currentPacket = new Holder<>();

    public void flush() throws IOException {
        LogsPacket toWrite = null;
        synchronized (currentPacket) {
            if (currentPacket.value != null && !currentPacket.value.blocks.isEmpty())
                toWrite = currentPacket.value;
            currentPacket.value = null;
        }
        if (toWrite != null)
            write(toWrite);
    }

    void add(TList<Log> items) throws IOException {

        for (Log log : items)
            synchronized (currentPacket) {

                if (currentPacket.value == null)
                    currentPacket.value = new LogsPacket(this);
                currentPacket.value.add(log, ++lastLogId);

                int totalSize = 0;
                for (byte[] buff : currentPacket.value.blocks)
                    totalSize += buff.length;

                if (totalSize > LogsStorage.RAW_PACKET_SIZE)
                    flush();
            }

    }
    //-------------------------------------------------------------------------

    public boolean read(LogsFileMeta meta, Long maxDate, LogEntry<?>[] attrs, LogsVisitor intf)
            throws IOException {

        beginRead();
        try {
            try (TInputStream in = new TInputStream(
                    new BufferedInputStream(new FileInputStream(file), BUFFER_SIZE))) {

                boolean checkPos = true;
                for (LogsPacket pck : header.packets) {
                    if (checkPos)
                        if (maxDate != null && pck.minDate > maxDate)
                            continue;

                    if (checkPos)
                        in.skipTo(pck.getPosition());

                    if (!pck.readData(in, meta, maxDate, attrs, intf))
                        return false;

                    checkPos = false;
                }
            }
        } finally {
            endRead();
        }

        return true;
    }

    public Log read(long logId) throws IOException {
        beginRead();
        try {
            try (TInputStream in = new TInputStream(
                    new BufferedInputStream(
                            new FileInputStream(file), BUFFER_SIZE))) {

                for (LogsPacket pck : header.packets)

                    if (logId >= pck.minId && logId <= pck.maxId) {

                        in.skipTo(pck.getPosition());

                        in.setLimit(pck.packetSize);

                        byte[] rawData = IOUtils.read(new InflaterInputStream(in));

                        TInputStream pckIn = new TInputStream(rawData);

                        for (int i = 0; i < pck.logsCount; i++) {

                            int blockSize = pckIn.readUnsignedDynInt();
                            byte[] block = pckIn.read(blockSize);

                            if (pck.maxId - i > logId)
                                continue;

                            Log log = pck.readLog(block, null);

                            return log;
                        }

                    }
            }
        } finally {
            endRead();
        }
        return null;
    }

    static LogsPacket writeAwaiting;

    private void write(LogsPacket pck) throws IOException {
        writeAwaiting = pck;

        if (!file.exists())
            file.createNewFile();

        File temp = null;
        // skompresuj wszystko
        pck.getCompressed();

        beginWrite();
        try {

            for (byte[] b : pck.blocks)
                header.totalRawSize += b.length;

            header.attributesCount += pck.attributesCount;
            header.logsCount += pck.logsCount;

            if (header.minDate > pck.minDate)
                header.minDate = pck.minDate;
            if (header.maxDate < pck.maxDate)
                header.maxDate = pck.maxDate;

            for (Entry<LogAttr, Counter<String>> en : pck.counters.entrySet()) {
                Counter<String> cntr = header.counters.get(en.getKey());
                if (cntr == null) {
                    cntr = new Counter<>();
                    header.counters.put(en.getKey(), cntr);
                }
                cntr.incAll(en.getValue());
            }

            //  TConsole.print("Zapisuję " + packets.size() + " pakietów");
            // weryfikacja ------------------------------------------------- usunac ------------------------------------------------
            /*    int readed = currentPacket.readData(new TInputStream(dBlock), this, null, null, null);
             if (readed != currentPacket.logsCount)
             throw new IOException("Błąd weryfikacji");
             */
//            long fSize = file.length();
            synchronized (header) {
                header.packets.add(0, pck);
            }

            byte[] bheader = header.build();

            temp = new Path(file).changeExtension("~mlog").toFile();
            try (TOutputStream out = new TOutputStream(temp)) {
                int oryginalHeaderSize = header.headerSize;
                long fSize = file.length();

                header.headerSize = bheader.length; // zapisz rozmiar nowego naglowka
                out.write(bheader);
                out.write(pck.getCompressed());

                if (fSize > 0)
                    try (TInputStream in = new TInputStream(file, BUFFER_SIZE)) {
                        in.skipTo(oryginalHeaderSize);
                        byte[] buffer = new byte[1024 * 100];
                        int read;
                        while ((read = in.read(buffer)) > 0)
                            out.write(buffer, 0, read);
                    }

//                TConsole.printTs("Zapisuję plik logów %s, rozmiar: %s, logów: %s, pakietów: %s, rozmiar: %s",
//                        file.getName(),
//                        Utils.formatSize(out.length()),
//                        Utils.formatValue(header.logsCount),
//                        Utils.formatValue(header.packets.size()),
//                        Utils.formatSize(out.length() - bheader.length)
//                        + " / " + Utils.formatSize(header.totalRawSize)
//                );
            }

            for (int i = 0; i < 10 && !file.delete(); i++)
                Utils.sleep(10);

            for (int i = 0; i < 100 && !temp.renameTo(file); i++)
                Utils.sleep(10);

            fileSize = file.length();

        } finally {
            endWrite();

            if (temp != null)
                temp.delete();

            writeAwaiting = null;
        }

        // opóźnienie, aby zapis nie odbywał się zbyt często
        Utils.sleep(1000);

    }

}

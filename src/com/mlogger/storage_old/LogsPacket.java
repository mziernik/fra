package com.mlogger.storage_old;

import com.utils.console.TConsole;
import com.utils.Utils;
import com.utils.Is;
import com.io.*;
import com.mlogger.*;
import com.mlogger.LogEntry.LogEntries;
import com.utils.Counter;
import com.utils.Is;
import com.utils.date.TDate;
import java.io.*;
import java.util.*;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;

@Deprecated
public class LogsPacket {

    private final LogAttr[] excluded = {LogAttr.UID, LogAttr.COUNTER, LogAttr.SOURCE_CODE};

    public final LogsFile logs;
    long minDate = Long.MAX_VALUE;
    long maxDate = Long.MIN_VALUE;
    long minId = Long.MAX_VALUE;
    long maxId = Long.MIN_VALUE;
    int packetSize;
    int rawPacketSize;
    int position;
    int logsCount;
    int attributesCount;

    final Map<LogAttr, Counter<String>> counters = new HashMap<>();

    final List<byte[]> blocks = new LinkedList<>();

    @Override
    public String toString() {
        return "Logów: " + logsCount
                + ", rozmiar: " + Utils.formatSize(packetSize) + ",\n"
                + new TDate(minDate) + " - " + new TDate(maxDate);
    }

    public byte[] build() throws IOException {
        TOutputStream out = new TOutputStream(true);
        out.writeUnsignedDyn(position);
        out.writeUnsignedDyn(packetSize);
        out.writeUnsignedDyn(logsCount);
        out.writeUnsignedDyn(attributesCount);
        out.writeUnsignedDyn(minId);
        out.writeUnsignedDyn(maxId);
        out.writeLong(minDate);
        out.writeLong(maxDate);
        return out.memory();
    }

    public int getPosition() {
        return position;
    }

    public LogsPacket(LogsFile logs) {
        this.logs = logs;
    }

    LogsPacket(LogsFile logs, TInputStream in) throws IOException {
        this.logs = logs;
        position = in.readUnsignedDynInt();
        packetSize = in.readUnsignedDynInt();
        logsCount = in.readUnsignedDynInt();
        attributesCount = in.readUnsignedDynInt();
        minId = in.readUnsignedDyn();
        maxId = in.readUnsignedDyn();
        minDate = in.readLong();
        maxDate = in.readLong();

    }

    /**
     *
     * @param src
     * @param logs
     * @param maxId
     * @param maxDate
     * @param visitor
     * @return Ilość zwróconych logów
     * @throws IOException
     */
    boolean readData(final TInputStream src, final LogsFileMeta meta,
            final Long maxDate, final LogEntry[] attrs, final LogsVisitor visitor)
            throws IOException {

        src.setLimit(packetSize);

        byte[] rawData = IOUtils.read(new InflaterInputStream(src));

        TInputStream in = new TInputStream(rawData);

        for (int i = 0; i < logsCount; i++) {

            int blockSize = in.readUnsignedDynInt();

            byte[] block = new byte[blockSize];
            in.readFully(block);

            Log log = readLog(block, attrs);

            if (maxDate != null && log.date.value().getTime() > maxDate - 1)
                continue;

            if (visitor != null)
                if (!visitor.onRead(meta, log))
                    return false;
        }
        return true;
    }

    private byte[] compressed;

    byte[] getCompressed() throws IOException {
        if (compressed == null) {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            try (TOutputStream out = new TOutputStream(new DeflaterOutputStream(bout))) {
                for (byte[] buff : blocks) {
                    rawPacketSize += buff.length;
                    out.writeUnsignedDyn(buff.length);
                    out.write(buff);
                }
                out.flush();
            }
            compressed = bout.toByteArray();
            packetSize = compressed.length;
        }
        return compressed;
    }

    void add(Log log, int id) throws IOException {

        TOutputStream attrOut = new TOutputStream(true);
        for (LogEntry<?> en : log.entries) {
            if (en.isEmpty() || Is.in(en.attr, excluded))
                continue;

            TOutputStream osEntry = new TOutputStream(true);
            en.write(osEntry, null);
            byte[] data = osEntry.memory();
            if (data == null || data.length == 0)
                continue;

            attrOut.write(en.attr.id); // bajt
            attrOut.writeUnsignedDyn(data.length);
            attrOut.write(data);
        }

        TOutputStream out = new TOutputStream(true);

        byte[] battrs = attrOut.memory();
        out.writeUnsignedDyn(id);
        out.writeLong(log.date.value().getTime());
        out.writeLong(log.date.value().getTime());
        out.write(log.level.value() != null ? log.level.value() : 0);
        out.writeUnsignedDyn(battrs.length);
        out.write(battrs);

        byte xor = 0;
        for (byte i : out.memory())
            xor ^= i;

        out.writeByte(xor);
        out.flush();
        byte[] block = out.memory();

        blocks.add(0, block);
        ++logsCount;

        long date = log.date.value().getTime();
        if (minDate > date)
            minDate = date;
        if (maxDate < date)
            maxDate = date;
        if (minId > id)
            minId = id;
        if (maxId < id)
            maxId = id;

        for (LogAttr attr : LogsStorage.indexedEntries) {
            LogEntry<?> entry = null;
            for (LogEntry<?> en : log.entries)
                if (en.id == attr.id) {
                    entry = en;
                    break;
                }

            if (entry == null)
                continue;

            List<String> list = new LinkedList<>();

            if (entry instanceof LogEntries)
                for (Object o : ((LogEntries<?>) entry).value())
                    list.add(Utils.toString(o));
            else
                list.add(Utils.toString(entry.value()));

            if (list.isEmpty())
                continue;

            Counter<String> counter = counters.get(attr);
            if (counter == null) {
                counter = new Counter<>();
                counters.put(attr, counter);
            }

            for (String s : list)
                if (!Is.empty(s))
                    counter.inc(s);

        }

    }

    private static class SLog extends Log {

    }

    Log readLog(byte[] data, LogEntry[] attrs) throws IOException {

        TInputStream in = new TInputStream(data);

        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        in.addMirror(bout);

        long id = in.readUnsignedDyn();
        long created = in.readLong();

        Log log = new SLog();
        log.date.set(new TDate(created));
        log.id = id;
        log.date.set(new TDate(in.readLong()));
        log.level.set(in.read());

        TInputStream attrIn = new TInputStream(in);
        long attrsSize = in.readUnsignedDyn();
        attrIn.setLimit(attrsSize);

        while (attrIn.available() > 0) {
            LogAttr attr = LogAttr.get(attrIn.readByte());
            int size = attrIn.readUnsignedDynInt();
            byte[] buff = attrIn.read(size);
            LogEntry<?> entry = log.getEntry(attr);
            if (entry != null)
                entry.read(buff);
            else
                TConsole.printTs("Nieobsługiwany typ logu");
        }

        in.removeMirror(bout);

        byte xor = 0;
        for (byte i : bout.toByteArray())
            xor ^= i;

        byte rxor = in.readByte();
        if (xor != rxor)
            throw new IOException("Nieprawidłowa suma kontrolna bloku");
        return log;
    }

}

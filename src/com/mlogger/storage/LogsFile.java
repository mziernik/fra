package com.mlogger.storage;

import com.context.AppConfig;
import com.intf.callable.Callable1;
import com.io.TInputStream;
import com.io.TOutputStream;
import com.mlogger.Log;
import com.mlogger.LogAttr;
import com.mlogger.LogEntry;
import com.utils.Is;
import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.TList;
import com.utils.date.TDate;
import java.io.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.Deflater;

public class LogsFile {

    final byte[] signature = "mLog".getBytes();
    byte version = 1;

    public UUID fileUid = UUID.randomUUID();
    public TDate created = new TDate();
    public TDate min = new TDate(0);
    public TDate max = new TDate(0);
    public long count = 0;
    public final File file;
    public final AtomicInteger id = new AtomicInteger(0);
    final static LogAttr[] EXCLUDED = {LogAttr.UID, LogAttr.COUNTER, LogAttr.SOURCE_CODE};
    private int headerSize;
    int size;
    public boolean isTemporary;
    public final TList<Index> index = new TList<>();
    public final RandomAccessFile raf;
    private int dataPos;

    public enum LogsFileFlags {
        TEMPORARY
    }

    private void write() {

    }

    LogsFile(File file) throws IOException {
        this.file = file;
        LogsStorage.files.add(this);
        isTemporary = !file.exists();

        raf = new RandomAccessFile(file, isTemporary ? "rw" : "r");

        if (isTemporary) {
            writeHeader();
            size = headerSize;
            return;
        }

        try (TInputStream in = new TInputStream(file, 1024 * 100)) {
            if (in.available() < 10)
                throw new IOException("Incorrect size");

            size = in.available();

            byte[] sig = in.read(signature.length);
            if (!Arrays.equals(signature, sig))
                throw new IOException("Incorrect signature");

            byte ver = in.readByte();
            if (version != ver)
                throw new IOException("Unsupporterd version (" + ver + ")");

            TList<LogsFileFlags> flags = TInputStream.flags(LogsFileFlags.class, in.readByte());
            isTemporary = flags.contains(LogsFileFlags.TEMPORARY);

            fileUid = new UUID(in.readLong(), in.readLong());
            created = new TDate(in.readLong());
            min = new TDate(in.readLong());
            max = new TDate(in.readLong());
            count = in.readUnsignedDyn();

            String name = in.readStringDyn();
            String title = in.readStringDyn();

            headerSize = (int) in.position();

            if (isTemporary) {
                min = new TDate(Long.MAX_VALUE);

                readAll((SLog log) -> {

                    count += 1;
                    long date = log.date.value().getTime();

                    if (date < min.getTime())
                        min = new TDate(date);

                    if (date > max.getTime())
                        max = new TDate(date);

                    return true;
                });
            }
            // offset = (int) out.length();
            LogsStorage.files.add(this);
        }

        //     compact();
    }

    void compact() throws IOException {

        TList<SLog> logs = new TList<>();

        long fileSize;
        synchronized (this) {
            try (TInputStream in = new TInputStream(file, 1024 * 100)) {
                fileSize = file.length();
                in.skipTo(headerSize);
                while (in.available() > 0) {
                    long pos = in.position();
                    byte[] data = in.read((int) in.readUnsignedDyn());
                    logs.add(new SLog(data));
                }

            }
        }

        ValuesDict dict = new ValuesDict();

        //-------------------------------------------------------------
        TOutputStream osAttrs = new TOutputStream(true);
        osAttrs.compreesion(new Deflater());
        // utwórz słownik
        for (Log log : logs)
            for (LogEntry<?> en : log.entries) {
                if (en.isEmpty() || Is.in(en.attr, EXCLUDED))
                    continue;
                en.write(osAttrs, dict);
            }
        osAttrs.close();

        //-------------------------------------------------------------
        TOutputStream osDict = new TOutputStream(true);
        osDict.compreesion(new Deflater());

        for (Entry<String, ValuesDict.DictEntry> en : dict.map.entrySet()) {
            byte[] data = en.getKey().substring(2).getBytes(Utils.UTF8);
            osDict.write(en.getValue().entry.id);
            osDict.write(data);
            osDict.write(0);
        }

        osDict.close();

        TOutputStream fout = new TOutputStream(new Path(file).changeExtension("m2").getFile());
        fout.write(osDict.memory());
        fout.write(osAttrs.memory());
        fout.close();

    }

    public void readAll(Callable1<Boolean, SLog> callback) throws IOException {
        TList<SLog> list = new TList<>();

        synchronized (this) {
            try (TInputStream in = new TInputStream(file, 1024 * 100)) {
                in.skipTo(headerSize);

                while (in.available() > 0) {

                    long pos = in.position();

                    byte[] data = in.read((int) in.readUnsignedDyn());

                    SLog log = new SLog(data);

                    index.add(0, new Index(log, pos));

                    if (isTemporary) {
                        list.add(0, log);
                        continue;
                    }

                    Boolean result = callback.run(log);
                    if (Boolean.FALSE.equals(result))
                        break;
                }

            }

            if (isTemporary)
                for (SLog log : list) {
                    Boolean result = callback.run(log);
                    if (Boolean.FALSE.equals(result))
                        break;
                }

        }
    }

    private void writeHeader() throws IOException {
        TOutputStream hdr = new TOutputStream(true);
        hdr.write(signature);
        hdr.writeByte(version);
        hdr.writeByte((byte) TOutputStream.flags(LogsFileFlags.TEMPORARY));
        hdr.writeLong(fileUid.getMostSignificantBits());
        hdr.writeLong(fileUid.getLeastSignificantBits());
        hdr.writeLong(created.getTime());
        hdr.writeLong(min.getTime());
        hdr.writeLong(max.getTime());
        hdr.writeUnsignedDyn(count);
        hdr.writeStringDyn(AppConfig.getServiceName());
        hdr.writeStringDyn(AppConfig.getServiceTitle());

        headerSize = (int) hdr.length();

        raf.seek(0);
        raf.write(hdr.memory());
    }

    void write(Log log) throws FileNotFoundException, IOException {

        size = headerSize;

        int id;
        synchronized (this.id) {
            id = this.id.incrementAndGet();
        }

        byte[] data = SLog.build(log, id);

        if (dataPos == 0) {
            // zerowanie
            int left = LogsStorage.MAX_FILE_SIZE;
            dataPos = left;
            while (left > 0) {
                byte[] buff = new byte[Utils.range(left, 0, 1024 * 100)];
                raf.write(buff);
                left -= buff.length;
            }
        }

        raf.seek(dataPos - data.length);
        raf.write(data);

    }

}

package com.mlogger.storage_old;

import com.utils.Utils;
import com.utils.Is;
import com.io.*;
import com.mlogger.LogAttr;
import com.utils.Counter;
import com.utils.date.TDate;
import java.io.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;

@Deprecated
public class LogsHeader {

    public final byte[] signature = "mL0G".getBytes();
    //-----------
    boolean fLocal = true; // w wersji lokalnej nie jest zapisywana data serwera
    boolean fClosed = false; // plik zamknięty, nie można go już edytować
    //====================
    byte version = 1;
    byte flags = 0;
    public int logsCount = 0;
    public int attributesCount = 0;
    public int totalRawSize = 0; // sumaryczny rozmiar nieskompresowanych danych
    public TDate created = new TDate();

    public long minDate = Long.MAX_VALUE;
    public long maxDate = Long.MIN_VALUE;
    int headerSize;

    public UUID fileUid = UUID.randomUUID();
    public final LogsFile logs;

    final LinkedHashMap<LogAttr, Counter<String>> counters = new LinkedHashMap<>();

    final List<LogsPacket> packets = new LinkedList<>();

    LogsHeader(LogsFile logs, TInputStream in) throws IOException {
        this.logs = logs;
        if (in == null)
            return;

        int fileSize = in.available();
        if (fileSize <= 10)
            return;

        byte[] buff = new byte[signature.length];
        in.readFully(buff);
        if (!Arrays.equals(buff, signature))
            throw new IOException("Nieprawidłowa sygnatura pliku " + logs.file.getName());

        version = in.readByte();
        flags = in.readByte();
        fileUid = new UUID(in.readLong(), in.readLong());
        created = new TDate(in.readLong());
        logsCount = in.readUnsignedDynInt();
        attributesCount = in.readUnsignedDynInt();
        totalRawSize = in.readUnsignedDynInt();
        minDate = in.readLong();
        maxDate = in.readLong();

        {
            int size = in.readUnsignedDynInt();
            byte[] rawData = IOUtils.read(new InflaterInputStream(new TInputStream(in).setLimit(size)));

            TInputStream inDict = new TInputStream(rawData);

            int countersCount = inDict.readUnsignedDynInt();

            for (int k = 0; k < countersCount; k++) {

                int entryId = inDict.readUnsignedDynInt();
                LogAttr attr = LogAttr.get(entryId);
                int entries = inDict.readUnsignedDynInt();

                Counter<String> cntr = new Counter<>();

                for (int i = 0; i < entries; i++) {
                    int cnt = inDict.readUnsignedDynInt();
                    byte[] val = inDict.read(inDict.readUnsignedDynInt());
                    cntr.put(new String(val, Utils.UTF8), cnt);
                }

                counters.put(attr, cntr);
            }

        }

        headerSize = in.readUnsignedDynInt();
        int pcksSize = in.readUnsignedDynInt();

        buff = new byte[pcksSize];
        in.readFully(buff);

        buff = IOUtils.read(new InflaterInputStream(new ByteArrayInputStream(buff)));

        TInputStream pckIn = new TInputStream(buff);
        while (pckIn.available() > 0) {
            LogsPacket pck = new LogsPacket(logs, pckIn);
            packets.add(pck);
        }

        if (in.position() > headerSize)
            throw new IOException("Nieprawidłowy rozmiar nagłówka ("
                    + in.position() + " <> " + headerSize);

        byte[] empty = new byte[headerSize - (int) in.position()];
        in.readFully(empty);

        for (byte b : empty)
            if (b != 0)
                throw new IOException("Non empty");

    }

    @Override
    public String toString() {

        return "Plik: " + logs.file.getName() + "\n"
                + "Rozmiar pliku: " + Utils.formatSize(logs.file.length()) + "\n"
                + "Rozmiar danych: " + Utils.formatSize(totalRawSize) + "\n"
                + "Ilość logów: " + Utils.formatValue(logsCount) + "\n"
                + "Ilosć wartości: " + Utils.formatValue(attributesCount) + "\n"
                + "Ilość pakietów: " + Utils.formatValue(packets.size()) + "\n"
                + "Zakres dat: "
                + (minDate != Long.MAX_VALUE ? new TDate(minDate).toString(false) : "-")
                + " - " + (maxDate != Long.MIN_VALUE ? new TDate(maxDate).toString(false) : "-");

    }

    public byte[] build() throws IOException {
        TOutputStream out = new TOutputStream(true);
        out.write(signature);
        out.writeByte(version);
        out.writeByte(flags);
        out.writeLong(fileUid.getMostSignificantBits());
        out.writeLong(fileUid.getLeastSignificantBits());
        out.writeLong(created.getTime());
        out.writeUnsignedDyn(logsCount);
        out.writeUnsignedDyn(attributesCount);
        out.writeUnsignedDyn(totalRawSize);
        out.writeLong(minDate);
        out.writeLong(maxDate);

        {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            try (TOutputStream dict = new TOutputStream(new DeflaterOutputStream(bout))) {

                dict.writeUnsignedDyn(counters.size());

                for (Entry<LogAttr, Counter<String>> element : counters.entrySet()) {
                    Counter<String> counter = element.getValue();
                    dict.writeUnsignedDyn(element.getKey().id); //23
                    dict.writeUnsignedDyn(counter.size()); //1
                    for (Entry<String, Integer> en : counter) {
                        byte[] val = en.getKey().getBytes(Utils.UTF8);
                        dict.writeUnsignedDyn(en.getValue());
                        dict.writeUnsignedDyn(val.length);
                        dict.write(val);
                    }
                }
                dict.flush();
            }

            out.writeUnsignedDyn(bout.size());
            out.write(bout.toByteArray());
        }

        ByteArrayOutputStream bout = new ByteArrayOutputStream();

        // przelicz wstepnie rozmiar naglowka
        try (DeflaterOutputStream dout = new DeflaterOutputStream(bout)) {
            int position = (int) out.length();
            for (LogsPacket block : packets) {
                block.position = position;
                dout.write(block.build());
                position += block.packetSize;
            }
            dout.flush();
        }

        int aproxSize = (int) out.length() + bout.size() + 100;

        bout.reset();
        try (DeflaterOutputStream dout = new DeflaterOutputStream(bout)) {
            int position = aproxSize;
            for (LogsPacket block : packets) {
                block.position = position;
                dout.write(block.build());
                position += block.packetSize;
            }
            dout.flush();
        }

        out.writeUnsignedDyn(aproxSize);
        out.writeUnsignedDyn(bout.size());
        out.write(bout.toByteArray());

        if (aproxSize > 0 && out.length() > aproxSize)
            throw new IOException(logs.file.getName() + ": Nieprawidłowy rozmiar nagłówka "
                    + out.length() + " > " + aproxSize);

        //      aproxSize -  out.length()
        while (out.length() < aproxSize)
            out.write(0);

        return out.memory();
    }

}

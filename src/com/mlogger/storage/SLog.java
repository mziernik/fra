package com.mlogger.storage;

import com.io.TInputStream;
import com.io.TOutputStream;
import com.mlogger.Log;
import com.mlogger.LogAttr;
import com.mlogger.LogEntry;
import com.utils.Is;
import com.utils.Utils;
import com.utils.Is;
import com.utils.console.TConsole;
import com.utils.date.TDate;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class SLog extends Log {

    public long attrsLength;

    SLog(byte[] data) throws IOException {

        TInputStream in = new TInputStream(data);

        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        in.addMirror(bout);

        long id = in.readUnsignedDyn();
        long created = in.readLong();

        date.set(new TDate(created));
        id = id;
        date.set(new TDate(in.readLong()));
        level.set(in.read());

        TInputStream attrIn = new TInputStream(in);
        attrsLength = in.readUnsignedDyn();

        attrIn.setLimit(attrsLength);

        while (attrIn.available() > 0) {
            LogAttr attr = LogAttr.get(attrIn.readByte());
            int size = attrIn.readUnsignedDynInt();
            byte[] buff = attrIn.read(size);
            LogEntry<?> entry = getEntry(attr);
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
    }

    static byte[] build(Log log, int id) throws IOException {
        TOutputStream attrOut = new TOutputStream(true);
        for (LogEntry<?> en : log.entries) {
            if (en.isEmpty() || Is.in(en.attr, LogsFile.EXCLUDED))
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

        TOutputStream block = new TOutputStream(true);

        byte[] battrs = attrOut.memory();
        block.writeUnsignedDyn(id);
        block.writeLong(log.date.value().getTime());
        block.writeLong(log.date.value().getTime());
        block.write(log.level.value() != null ? log.level.value() : 0);
        block.writeUnsignedDyn(battrs.length);
        block.write(battrs);

        byte xor = 0;
        for (byte i : block.memory())
            xor ^= i;

        block.writeByte(xor);
        block.flush();

        return block.memory();
    }
}

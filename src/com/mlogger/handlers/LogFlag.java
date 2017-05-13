package com.mlogger.handlers;

import java.util.LinkedHashSet;
import com.mlogger.LogDefaults;
import com.mlogger.LogOptions;

public enum LogFlag {

    token(0x01),
    multipart(0x02),
    compressed(0x04),
    encrypted(0x08),
    priority(0x10);

    private LogFlag(int id) {
        this.value = (byte) id;
    }

    public final byte value;

    public static class LogFlags extends LinkedHashSet<LogFlag> {

        public byte toByte() {
            byte b = 0;
            for (LogFlag lf : this)
                if (lf != null)
                    b += lf.value;
            return b;
        }

        public LogFlags(LogOptions options) {
            super();
            if (options == null)
                return;

            if (options.priority)
                add(LogFlag.priority);

            if (options.token != null)
                add(LogFlag.token);

            if (options.compressed)
                add(LogFlag.compressed);

            if (options.encryptionKey != null && options.token != null)
                add(LogFlag.encrypted);

        }

        public void fromByte(byte b) {
            clear();
            for (LogFlag lf : LogFlag.values())
                if ((lf.value & b) == lf.value)
                    add(lf);
        }
    }

}

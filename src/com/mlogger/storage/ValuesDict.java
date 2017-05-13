package com.mlogger.storage;

import com.mlogger.Log;
import com.mlogger.LogEntry;
import com.utils.hashes.Hex;
import java.util.LinkedHashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class ValuesDict {

    final AtomicInteger idCounter = new AtomicInteger(0);

    final LinkedHashMap<String, DictEntry> map = new LinkedHashMap<>();

    public DictEntry get(Log log, LogEntry<?> le, String value) {

        value = Hex.toString(le.id, 1) + value;

        DictEntry entry = map.get(value);
        if (entry == null)
            entry = new DictEntry(log, le, value);
        ++entry.count;
        return entry;
    }

    public class DictEntry {

        public Log log;
        public LogEntry<?> entry;
        public int id;
        public int count;

        DictEntry(Log log, LogEntry<?> entry, String value) {
            this.log = log;
            this.entry = entry;
            map.put(value, this);
            synchronized (idCounter) {
                this.id = idCounter.incrementAndGet();
            }
        }

    }
}

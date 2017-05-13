package com.mlogger.storage_old;

import com.mlogger.LogAttr;
import com.mlogger.storage_old.LogsFileMeta.IndexEntry;
import com.utils.Counter;
import com.utils.date.TDate;
import java.io.File;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Consumer;

@Deprecated
public class LogsFileMeta implements Iterable<IndexEntry> {

    public final List<IndexEntry> index = new LinkedList<>();

    public final UUID uid;

    public final byte version;
    public final byte flags;
    public final int logsCount;
    public final int attributesCount;
    public final int totalRawSize; // sumaryczny rozmiar nieskompresowanych danych
    public final TDate created;

    public final TDate minDate;
    public final TDate maxDate;
    public final File file;

    public LogsFileMeta(LogsFile file) {
        this.file = file.file;
        this.uid = file.header.fileUid;
        this.version = file.header.version;
        this.flags = file.header.flags;
        this.logsCount = file.header.logsCount;
        this.attributesCount = file.header.attributesCount;
        this.totalRawSize = file.header.totalRawSize;
        this.created = file.header.created;
        this.minDate = new TDate(file.header.minDate);
        this.maxDate = new TDate(file.header.maxDate);

        for (Entry<LogAttr, Counter<String>> en1 : file.header.counters.entrySet())
            for (Entry<String, Integer> en2 : en1.getValue())
                new IndexEntry(en1.getKey(), en2.getKey(), en2.getValue());
    }

    @Override
    public Iterator<IndexEntry> iterator() {
        return index.iterator();
    }

    public Map<String, Integer> getIndex(final LogAttr logAttr) {
        final Map<String, Integer> map = new LinkedHashMap<>();
        index.stream().forEach((IndexEntry entry) -> {
            if (entry.attr == logAttr)
                map.put(entry.phrase, entry.count);
        });
        return map;
    }

    public class IndexEntry {

        public final LogAttr attr;
        public final String phrase;
        public final int count;

        private IndexEntry(LogAttr attr, String phrase, int count) {
            this.attr = attr;
            this.phrase = phrase;
            this.count = count;
            index.add(this);
        }

    }

}

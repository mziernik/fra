package com.mlogger.storage;

import com.mlogger.Log;
import com.mlogger.LogKind;
import java.util.Collection;

public class Index {

    class DictEntry {

        String value;
        int crc;
        int id;
    }

    long id;
    long date;
    long pos;
    Log log;
    Long process;
    Long thread;
    Object value;
    LogKind kind;
    Collection<String> tags;
    Collection<String> address;

    Index(SLog log, long pos) {
        this.pos = pos;
        this.log = log;
        this.id = log.id;
        this.kind = log.kind.value();
        this.value = log.value.value().value;
        this.tags = log.tag.value();
        this.address = log.address.value();
        this.date = log.date.value().getTime();
        this.process = log.processId.value();
        this.thread = log.threadId.value();
    }

}

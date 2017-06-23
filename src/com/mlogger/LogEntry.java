package com.mlogger;

import com.config.CService.ServiceMode;
import com.utils.Utils;
import com.utils.Is;
import com.io.*;
import com.mlogger.LogElement.DataObj;
import com.mlogger.LogElement.DataPair;
import com.mlogger.LogElement.DataPairs;
import com.mlogger.storage.ValuesDict;
import com.utils.date.TDate;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.Map.Entry;

public class LogEntry<T> {

    public final LogAttr attr;

    public final int id;
    public final String title;
    public final String name;
    public final String key;
    public final boolean required;
    public final boolean multiple;

    public int maxValueLength = 10000; // maksymalna dlugosc wartosci
    public static int maxStackTraceItemsCount = 100;

    protected final Log log;
    T value;

    @Override
    public String toString() {
        return attr.name + " = " + Utils.toString(value);
    }

    LogEntry(Log log, LogAttr attr) {
        this.log = log;
        this.attr = attr;
        this.id = attr.id;
        this.title = attr.title;
        this.key = attr.key;
        this.required = attr.required;
        this.multiple = this instanceof LogEntries || this instanceof LogEntryMap;
        this.name = attr.name;
        log.entries.add(this);
        assert this.multiple == attr.multiple;
    }

    public Log set(T value) {
        this.value = value;
        return log;
    }

    public T value(T defaultValue) {
        return value != null ? value : defaultValue;
    }

    public T value() {
        return this.value;
    }

    public boolean isEmpty() {
        return Is.empty(value);
    }

    // LogAttr[] excluded = {LogAttr.UID, LogAttr.COUNTER, LogAttr.SOURCE_CODE};
    public void write(TOutputStream out, ValuesDict dict) throws IOException {

        switch (attr) {

            case LOGGER:
            case CLAZZ:
            case METHOD:
            case LEVEL_NAME:
            case SOURCE:
            case DEVICE:
            case OS:
            case HOST_NAME:
            case USER_AGENT:
            case USER_NAME:
            case COMMENT:
            case INSTANCE:
            case SESSION:
            case REQUEST:
            case VERSION:
            case THREAD_NAME:
            case COLOR:
            case BACKGROUND:
            case FIELD_KEY:
            case GROUP:
                writeStr(out, value, dict);
                return;

            case UID: {
                UUID uuid = (UUID) value;
                ByteBuffer buffer = ByteBuffer.allocate(16);
                buffer.putLong(uuid.getMostSignificantBits());
                buffer.putLong(uuid.getLeastSignificantBits());
                out.write(buffer.array());
                return;
            }

            case VALUE: {
                DataObj dobj = (DataObj) value;
                out.write(Utils.coalesce(dobj.type, DataType_old.TEXT).id);
                writeStr(out, dobj.value, dict);
                return;
            }

            case DATE: {
                ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
                buffer.putLong(((Date) value).getTime());
                out.write(buffer.array());
                return;
            }

            case LEVEL:
            case THREAD_PRIORITY:
                out.writeSignedDyn((Integer) value);
                return;

            case PROCESS:
            case THREAD:
            case COUNTER:
                out.writeSignedDyn((Long) value);
                return;

            case PROGRESS:
                ByteBuffer buffer = ByteBuffer.allocate(Double.BYTES);
                buffer.putDouble((Double) value);
                out.write(buffer.array());
                return;

            case KIND:
                out.write(((LogKind) value).key);
                return;

            case MODE:
                out.write(((ServiceMode) value).key);
                return;

            case SOURCE_CODE:
                return;

            default:
                throw new UnsupportedOperationException("Write: " + attr);

        }
    }

    void writeStr(TOutputStream out, Object val, ValuesDict dict) throws IOException {

        if (dict != null) {

            char[] chars = Utils.coalesce(Utils.toString(val), "").toCharArray();
            StringBuilder sb = new StringBuilder();

            for (char c : chars) {
                if (c == '\n') {
                    out.writeUnsignedDyn(dict.get(log, this, sb.toString()).id);
                    out.write(c);
                    sb = new StringBuilder();
                    continue;
                }
                sb.append(c);
            }

            out.writeUnsignedDyn(dict.get(log, this, sb.toString()).id);
            out.write('0');

            return;
        }

        byte[] buff = Utils.coalesce(Utils.toString(val), "").getBytes(Utils.UTF8);
        out.writeUnsignedDyn(buff.length);
        out.write(buff);
    }

    String readStr(TInputStream in) throws IOException {
        int strLen = in.readUnsignedDynInt();
        byte[] buff = new byte[strLen];
        in.read(buff);
        return new String(buff, Utils.UTF8);
    }

    public void read(byte[] data) throws IOException {

        switch (attr) {

            case LOGGER:
            case CLAZZ:
            case METHOD:
            case LEVEL_NAME:
            case SOURCE:
            case DEVICE:
            case OS:
            case HOST_NAME:
            case USER_AGENT:
            case USER_NAME:
            case COMMENT:
            case INSTANCE:
            case SESSION:
            case REQUEST:
            case VERSION:
            case THREAD_NAME:
            case COLOR:
            case BACKGROUND:
            case FIELD_KEY:
            case GROUP:
                value = (T) new String(data, Utils.UTF8);
                break;

            case UID:
                value = (T) new UUID(ByteBuffer.wrap(data).getLong(0),
                        ByteBuffer.wrap(data).getLong(1));
                break;

            case DATE:
                value = (T) new TDate(ByteBuffer.wrap(data).getLong());
                break;

            case LEVEL:
            case THREAD_PRIORITY:
                value = (T) (Integer) DynValue.readSignedInt(new ByteArrayInputStream(data));
                break;

            case PROCESS:
            case THREAD:
            case COUNTER:
                value = (T) (Long) DynValue.readSigned(new ByteArrayInputStream(data));
                break;

            case PROGRESS:
                value = (T) (Double) ByteBuffer.wrap(data).getDouble();
                break;

            case KIND:
                value = (T) LogKind.get((char) data[0]);
                break;

            case MODE:
                value = (T) ServiceMode.get((char) data[0]);
                break;

            case VALUE:
                DataType_old dt = DataType_old.get(data[0]);
                value = (T) new DataObj("", new String(
                        Arrays.copyOfRange(data, 1, data.length), Utils.UTF8), dt);
                break;

            default:
                throw new UnsupportedOperationException("Read: " + attr);
        }
    }

    public static class LogEntries<T> extends LogEntry<Collection<T>> implements Iterable<T> {

        @Override
        public void read(byte[] data) throws IOException {
            TInputStream in = new TInputStream(data);

            value.clear();

            switch (attr) {
                case KEYS:
                case ADDRESS:
                case TAG:
                case URL:
                case CALL_STACK:
                case FLAGS: {
                    while (in.available() > 0)
                        add((T) readStr(in));
                    break;
                }
                case ERROR_STACK: {
                    while (in.available() > 0) {
                        int elements = in.readUnsignedDynInt();
                        List<String> stack = new LinkedList<>();
                        value.add((T) stack);
                        for (int j = 0; j < elements; j++)
                            stack.add(readStr(in));
                    }

                    break;
                }
                case DATA: {
                    while (in.available() > 0) {
                        DataType_old dt = DataType_old.get(in.readByte());
                        String name = readStr(in);
                        String val = readStr(in);
                        add((T) new DataObj(name, val, dt));
                    }
                    break;
                }

                default:
                    throw new UnsupportedOperationException("Read: " + attr);
            }

        }

        @Override
        public void write(TOutputStream out, ValuesDict dict) throws IOException {

            switch (attr) {
                case ERROR_STACK:
                    for (Collection<String> stack : (Collection<Collection<String>>) value) {
                        out.writeUnsignedDyn(stack.size());
                        for (String s : stack)
                            writeStr(out, s, dict);
                    }
                    break;

                case DATA:
                    for (DataObj pair : (Collection<DataObj>) value) {
                        out.write(Utils.coalesce(pair.type, DataType_old.TEXT).id);
                        writeStr(out, pair.name, dict);
                        writeStr(out, pair.value, dict);
                    }
                    break;

                case URL:
                case TAG:
                case KEYS:
                case ADDRESS:
                case FLAGS:
                case CALL_STACK:
                    for (Object o : value)
                        writeStr(out, o, dict);

                    break;

                default:
                    throw new UnsupportedOperationException("Write: " + attr);

            }
        }

        LogEntries(Log log, LogAttr attr) {
            super(log, attr);
            set(new LinkedHashSet<>());
        }

        public Log add(T value) {
            super.value.add(value);
            return log;
        }

        public Log addAll(Collection<? extends T> value) {
            super.value.addAll(value);
            return log;
        }

        public Log clear() {
            super.value.clear();
            return log;
        }

        @Override
        public boolean isEmpty() {
            return value == null || value.isEmpty();
        }

        @Override
        public Iterator<T> iterator() {
            return value.iterator();
        }

        public boolean contains(T s) {
            return value != null && value.contains(s);
        }

    }

    public static class LogEntryMap<T> extends LogEntry<Map<String, T>>
            implements Iterable<Entry<String, T>> {

        @Override
        public void read(byte[] data) throws IOException {

            TInputStream in = new TInputStream(data);

            if (attr != LogAttr.ATTRIBUTE)
                throw new UnsupportedOperationException("Read: " + attr);
            int count = in.readUnsignedDynInt();

            for (int i = 0; i < count; i++) {
                String name = readStr(in);
                int pairsCount = in.readUnsignedDynInt();
                DataPairs pairs = new DataPairs();
                for (int j = 0; j < pairsCount; j++)
                    pairs.add(new DataPair(readStr(in), readStr(in)));
                ((Map<String, DataPairs>) value).put(name, pairs);
            }
        }

        @Override
        public void write(TOutputStream out, ValuesDict dict) throws IOException {
            if (attr != LogAttr.ATTRIBUTE)
                throw new UnsupportedOperationException("Write: " + attr);

            Map<String, DataPairs> map = (Map<String, DataPairs>) value;

            out.writeUnsignedDyn(map.size());

            for (Entry<String, DataPairs> en : map.entrySet()) {
                DataPairs pairs = en.getValue();
                writeStr(out, en.getKey(), dict); // zapisz nazwę
                out.writeUnsignedDyn(pairs.size());
                for (LogElement.DataPair pair : pairs) {
                    writeStr(out, pair.name, dict);
                    writeStr(out, pair.value, dict);
                }
            }
        }

        LogEntryMap(Log log, LogAttr attr) {
            super(log, attr);
            set(new LinkedHashMap<>());
        }

        public Log add(String key, T value) {
            super.value.put(key, value);
            return log;
        }

        public Log addAll(Map<? extends String, ? extends T> value) {
            super.value.putAll(value);
            return log;
        }

        public Log clear() {
            super.value.clear();
            return log;
        }

        public T get(String key) {
            return value != null ? value.get(key) : null;
        }

        @Override
        public Iterator<Entry<String, T>> iterator() {
            return value.entrySet().iterator();
        }

        @Override
        public boolean isEmpty() {
            return value == null || value.isEmpty();
        }

    }

}

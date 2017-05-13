package com.mlogger.utils;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.Collection;
import com.mlogger.LogElement;
import com.mlogger.LogEntry;
import com.mlogger.LogEntry.LogEntries;

public class JsonBuilder {

    private final StringWriter wr = new StringWriter();
    private boolean newObj = true;
    public String nameQuota = null;

    public final byte[] getBytes() {
        return toString().getBytes(Charset.forName("UTF-8"));
    }

    @Override
    public String toString() {
        return wr.toString();
    }

    public JsonBuilder write(String str) {
        wr.append(str);
        return this;
    }

    public JsonBuilder obj(char ch) {
        if (ch != '}' && ch != ']' && ch != '{' && ch != '[')
            throw new RuntimeException("Niedozwolony znak \"" + ch + "\"");
        boolean no = ch == '{' || ch == '[';
        if (no && !newObj)
            wr.append(",");
        wr.append(ch);
        newObj = no;
        return this;
    }

    /*
     public JsonBuilder value(LogElement.LogEntry<?> at, Object value) throws IOException {
     if (at.value == null)
     return this;
     return name(at.key).value(at.value, at.maxLength);
     }
     */
    public JsonBuilder pair(String name, Object value, int maxLen) {
        if (value != null && !value.toString().isEmpty())
            name(name).value(value, maxLen);
        return this;
    }

    public JsonBuilder pair(LogEntry<?> attr) {
        return pair(attr, attr.value());
    }

    public JsonBuilder pair(LogEntry<?> attr, Object value) {
        if (value != null && !value.toString().isEmpty())
            name(attr.key).value(value, attr.maxValueLength);
        return this;
    }

    public JsonBuilder array(LogEntries<?> attr) {
        return array(attr, attr.value());
    }

    public JsonBuilder array(LogEntries<?> attr, Collection<?> values) {
        if (values != null && !values.isEmpty()) {
            name(attr.key).obj('[');
            for (Object o : values)
                value(o, attr.maxValueLength);
            obj(']');
        }
        return this;
    }

    public JsonBuilder dataObj(LogElement.DataObj data, LogEntry<?> attr) {
        if (data != null) {
            obj('[');
            value(data.type != null ? data.type : "", 100);
            value(data.name != null ? data.name : "", 300);
            value(data.value, attr.maxValueLength);
            obj(']');
        }
        return this;
    }

    public JsonBuilder dataPair(LogElement.DataPair pair, LogEntry<?> attr) {
        if (pair != null) {
            obj('[');
            value(pair.name != null ? pair.name : "", 300);
            value(pair.value, attr.maxValueLength);
            obj(']');
        }
        return this;
    }

    public JsonBuilder name(String str) {
        if (!newObj)
            wr.append(",");

        wr.append(nameQuota != null ? nameQuota : "")
                .append(str)
                .append(nameQuota != null ? nameQuota : "")
                .append(":");
        newObj = true;
        return this;
    }

    public JsonBuilder nameEsc(String str) {
        if (!newObj)
            wr.append(",");
        wr.append("\"");
        escapeJson(wr, str);
        wr.append("\":");
        newObj = true;
        return this;
    }

    public JsonBuilder value(Object value) {
        return value(value, 0);
    }

    public JsonBuilder value(Object value, int maxLen) {
        if (value == null)
            return this;
        if (!newObj)
            wr.append(",");
        newObj = false;

        String str = value.toString();

        if (maxLen > 0 && str.length() > maxLen)
            str = str.substring(0, maxLen - 3) + "[…]";

        if (value instanceof Boolean || value instanceof Number)
            wr.append(str);
        else {
            wr.append("\"");
            escapeJson(wr, str);
            wr.append("\"");
        }
        return this;
    }

    private static void escapeJson(Writer wr, String string) {
        if (string == null || string.length() == 0)
            return;
        char b;
        char c = 0;
        String hhhh;
        int i;
        int len = string.length();

        for (i = 0; i < len; i += 1)
            try {
                b = c;
                c = string.charAt(i);
                switch (c) {
                    case '\\':
                    case '"':
                        wr.append('\\');
                        wr.write(c);
                        break;
                    case '/':
                        if (b == '<')
                            wr.write('\\');
                        wr.write(c);
                        break;
                    case '\b':
                        wr.write("\\b");
                        break;
                    case '\t':
                        wr.write("\\t");
                        break;
                    case '\n':
                        wr.write("\\n");
                        break;
                    case '\f':
                        wr.write("\\f");
                        break;
                    case '\r':
                        wr.write("\\r");
                        break;
                    default:
                        if (c < ' ' || (c >= '\u0080' && c < '\u00a0')
                                || (c >= '\u2000' && c < '\u2100')) {
                            wr.write("\\u");
                            hhhh = Integer.toHexString(c);
                            wr.write("0000", 0, 4 - hhhh.length());
                            wr.write(hhhh);
                        } else
                            wr.write(c);
                }
            } catch (IOException ex) {
            }
    }

}

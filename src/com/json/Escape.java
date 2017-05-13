package com.json;

import com.exceptions.ThrowableException;
import com.mlogger.Log;
import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import java.io.*;

public class Escape {

    private boolean escapeUnicode;
    private Boolean useQuota;
    private boolean singleQuota = false;

    public Escape singleQuota(boolean singleQuota) {
        this.singleQuota = singleQuota;
        return this;
    }

    public Escape escapeUnicode(boolean escapeUnicode) {
        this.escapeUnicode = escapeUnicode;
        return this;
    }

    public Escape useQuota(Boolean useQuota) {
        this.useQuota = useQuota;
        return this;
    }

    public static String escape(Object object) {
        return new Escape().toString(object);
    }

    public static String unquoted(Object object) {
        return new Escape().useQuota(false).toString(object);
    }

    public String toString(Object object) {
        StringWriter sw = new StringWriter();
        try {
            toString(object, sw);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
        return sw.toString();
    }

    public static String js(Object object) {
        return new Escape().singleQuota(true).toString(object);
    }

    public static String jsUnquoted(Object object) {
        return new Escape().useQuota(false).toString(object);
    }

    public static StrWriter js(Object object, StrWriter w) {
        try {
            new Escape().singleQuota(true).toString(object, w);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
        return w;
    }

    public static StringWriter js(Object object, StringWriter w) {
        try {
            new Escape().singleQuota(true).toString(object, w);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
        return w;
    }

    public static Appendable js(Object object, Appendable w) throws IOException {
        return new Escape().singleQuota(true).toString(object, w);
    }

    public Appendable toString(Object object, Appendable w) throws IOException {

        if (object == null) {
            w.append("null");
            return w;
        }

        char quotaChar = singleQuota ? '\'' : '\"';

        boolean qt = useQuota == null
                ? qt = !(object instanceof Boolean || object instanceof Number)
                : useQuota;

        if (qt)
            w.append(quotaChar);

        String string = Utils.toString(object);

        if (string != null && !string.isEmpty()) {
            // char b;
            char c = 0;
            String hhhh;
            int i;
            int len = string.length();

            for (i = 0; i < len; i += 1) {
                //   b = c;
                c = string.charAt(i);

                if (c == quotaChar) {
                    w.append('\\').append(c);
                    continue;
                }

                switch (c) {
                    case '\\':
                        w.append('\\').append(c);
                        break;
//                    case '/':
//                        if (b == '<')
//                            w.append('\\');
//                        w.append(c);
//                        break;
                    case '\b':
                        w.append("\\b");
                        break;
                    case '\t':
                        w.append("\\t");
                        break;
                    case '\n':
                        w.append("\\n");
                        break;
                    case '\f':
                        w.append("\\f");
                        break;
                    case '\r':
                        w.append("\\r");
                        break;
                    default:
                        if (c < 32 || (escapeUnicode && c > 127)) {
                            w.append("\\u");
                            hhhh = Integer.toHexString(c);
                            w.append("0000", 0, 4 - hhhh.length());
                            w.append(hhhh);
                        } else
                            w.append(c);
                }
            }
        }

        if (qt)
            w.append(quotaChar);

        return w;
    }

    public static String format(String string, Object... params) {
        if (params != null && string != null)
            for (Object p : params)
                string = string.replaceFirst("%s", escape(p));
        return string;
    }

    public static String unescape(String str) {
        try {
            return unescape(str, new StringWriter()).toString();
        } catch (IOException ex) {
            Log.error(ex);
            return null;
        }
    }

    public static Appendable unescape(String str, Appendable writer) throws IOException {

        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (ch == '\\') {
                char nextChar = (i == str.length() - 1) ? '\\' : str
                        .charAt(i + 1);
                // Octal escape?
                if (nextChar >= '0' && nextChar <= '7') {
                    String code = "" + nextChar;
                    i++;
                    if ((i < str.length() - 1) && str.charAt(i + 1) >= '0'
                            && str.charAt(i + 1) <= '7') {
                        code += str.charAt(i + 1);
                        i++;
                        if ((i < str.length() - 1) && str.charAt(i + 1) >= '0'
                                && str.charAt(i + 1) <= '7') {
                            code += str.charAt(i + 1);
                            i++;
                        }
                    }
                    writer.append((char) Integer.parseInt(code, 8));
                    continue;
                }
                switch (nextChar) {
                    case '\\':
                        ch = '\\';
                        break;
                    case 'b':
                        ch = '\b';
                        break;
                    case 'f':
                        ch = '\f';
                        break;
                    case 'n':
                        ch = '\n';
                        break;
                    case 'r':
                        ch = '\r';
                        break;
                    case 't':
                        ch = '\t';
                        break;
                    case '\"':
                        ch = '\"';
                        break;
                    case '\'':
                        ch = '\'';
                        break;
                    // Hex Unicode: u????
                    case 'u':
                        if (i >= str.length() - 5) {
                            ch = 'u';
                            break;
                        }
                        int code = Integer.parseInt(
                                "" + str.charAt(i + 2) + str.charAt(i + 3)
                                + str.charAt(i + 4) + str.charAt(i + 5), 16);
                        writer.append(new String(Character.toChars(code)));
                        i += 5;
                        continue;
                }
                i++;
            }
            writer.append(ch);
        }
        return writer;
    }

}

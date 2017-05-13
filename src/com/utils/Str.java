package com.utils;

import com.utils.text.StrWriter;
import com.json.Escape;
import com.mlogger.Log;
import com.utils.collections.TList;
import java.io.*;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.zip.InflaterInputStream;

public class Str implements CharSequence {

    private final TList<String> prefix = new TList<>();
    private final TList<String> sufix = new TList<>();

    private final TList<String> removePrefix = new TList<>();
    private final TList<String> removeSufix = new TList<>();

    private String string;
    private String defValue = "";
    private String quotes = null;
    private boolean trim = true;
    //--------------------------------------

    private boolean caseSensitive = true;

    public Str(Object string) {
        this.string = Utils.toString(string);
    }

    /**
     * Konwersja tablicy bajtów do Stringa z obsługą BOM
     *
     * @param data
     * @param encoding
     * @return
     */
    public static String decode(byte[] data, Charset encoding) {
        if (data == null)
            return null;

        if (data.length == 0)
            return "";

        if (encoding == null || encoding.name().toUpperCase().contains("UTF"))
            for (Entry<byte[], Charset> en : Utils.BOM.entrySet()) {
                byte[] bom = en.getKey();
                if (data.length >= bom.length && Arrays.equals(bom, Arrays.copyOf(data, bom.length)))
                    return new String(data, bom.length, data.length - bom.length, en.getValue());

            }

        return new String(data, encoding);
    }

    // obluga BOM-a dla UTF-8
    public static BufferedInputStream decode(InputStream in, Charset encoding) {

        return new BufferedInputStream(in) {

            boolean first = true;

            @Override
            public synchronized int read(byte[] cbuf, int off, int len) throws IOException {
                int read = super.read(cbuf, off, len);

                if (read > 0 && first) {
                    byte[] data = Arrays.copyOf(cbuf, read > 6 ? 6 : read);

                    if (encoding == null || encoding.name().toUpperCase().contains("UTF"))
                        for (Entry<byte[], Charset> en : Utils.BOM.entrySet()) {
                            byte[] bom = en.getKey();
                            if (data.length >= bom.length && Arrays.equals(bom, Arrays.copyOf(data, bom.length))) {

                                for (int i = 0; i < read - bom.length; i++)
                                    cbuf[i] = cbuf[i + bom.length];
                                read -= bom.length;
                                break;
                            }

                        }

                    first = false;
                }

                return read;
            }

        };
    }

    public Str caseSensitive(boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
        return this;
    }

    public Str trim(boolean trim) {
        this.trim = trim;
        return this;
    }

    public Str defValue(String defValue) {
        this.defValue = defValue;
        return this;
    }

    public Str quotes(String quotes) {
        this.quotes = quotes;
        return this;
    }

    public Str prefix(String prefix) {
        if (prefix != null)
            this.prefix.add(prefix);
        return this;
    }

    public Str removePrefix(String removePrefix) {
        if (removePrefix != null)
            this.removePrefix.add(removePrefix);
        return this;
    }

    public Str sufix(String sufix) {
        if (sufix != null)
            this.sufix.add(sufix);
        return this;
    }

    public Str removeSufix(String removeSufix) {
        if (removeSufix != null)
            this.removeSufix.add(removeSufix);
        return this;
    }

    @Override
    public String toString() {

        String str = trim && string != null ? string.trim() : string;

        if (str == null)
            str = defValue;

        if (str == null)
            return null;

        if (!(quotes != null
                || prefix != null
                || sufix != null
                || removePrefix != null
                || removeSufix != null))
            return str; // nie modyfikujemy

        StrWriter sb = new StrWriter();

        if (quotes != null)
            sb.append(quotes);

        for (String s : prefix)
            if (!str.startsWith(s))
                sb.append(prefix);

        for (String s : removePrefix)
            if (str.startsWith(s))
                str = str.substring(s.length());

        for (String s : removeSufix)
            if (str.endsWith(s))
                str = str.substring(0, str.length() - s.length());

        sb.append(str);

        for (String s : sufix)
            if (!str.endsWith(s))
                sb.append(s);

        if (quotes != null)
            sb.append(quotes);

        return sb.toString();
    }

    public boolean matches(String mask) {
        return matchesMask(toString(), mask);
    }

    public static boolean matchesMask(String fileName, String mask) {
        if (mask == null || fileName == null)
            return false;
        mask = mask.toLowerCase();
        String special = "\\[]^$.|+-(){}";

        fileName = fileName.toLowerCase();

        for (char c : special.toCharArray())
            mask = mask.replace("" + c, "\\" + c);

        mask = mask.replace("?", ".").replace("*", ".*");

        try {
            return fileName.matches(mask);
        } catch (java.util.regex.PatternSyntaxException e) {
            Log.warning(e);
            return false;
        }
    }

    public String[] split(String regex) {
        return toString().split(regex);
    }

    /**
     *
     * @return Zwracany jest przykładowy tekst (licencja GNU w wersji polskiej i
     * angielskiej), przydatne w testach, jako losowa treść
     */
    public static LinkedList<String> getWordsDict() {
        LinkedList<String> lst = new LinkedList<>();

        try (BufferedReader in = new BufferedReader(
                new InputStreamReader(
                        new InflaterInputStream(
                                new BufferedInputStream(
                                        Escape.class
                                                .getResourceAsStream(
                                                        "/res/utils/dict.bin")
                                )
                        )
                )
        )) {
            String line;
            while ((line = in.readLine()) != null)
                lst.add(line);
        } catch (IOException e) {
            Log.error(e);
        }

        return lst;

    }

    /**
     * Czy zaczyna się od jednej z fraz
     *
     * @param parts
     * @return
     */
    public boolean startsWith(String... parts) {
        final String str = caseSensitive ? toString() : toString().toLowerCase();

        if (parts != null)
            for (String s : parts)
                if (str.startsWith(caseSensitive ? s : s.toLowerCase()))
                    return true;

        return false;
    }

    /**
     * Czy kończy się jedną z fraz
     *
     * @param parts
     * @return
     */
    public boolean endsWith(String... parts) {
        final String str = caseSensitive ? toString() : toString().toLowerCase();

        if (parts != null)
            for (String s : parts)
                if (str.endsWith(caseSensitive ? s : s.toLowerCase()))
                    return true;

        return false;
    }

    /**
     * Czy zawiera jedną z fraz
     *
     * @param parts
     * @return
     */
    public boolean contains(String... parts) {
        final String str = caseSensitive ? toString() : toString().toLowerCase();

        if (parts != null)
            for (String s : parts)
                if (str.contains(caseSensitive ? s : s.toLowerCase()))
                    return true;

        return false;
    }

    @Override
    public int length() {
        return string != null ? string.length() : -1;
    }

    @Override
    public char charAt(int index) {
        return string != null ? string.charAt(index) : null;
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        return string != null ? string.subSequence(start, end) : null;
    }

    public Str insert(int index, String value) {
        if (string != null && index >= 0 && index < string.length())
            string = string.substring(0, index) + value + string.substring(index);
        return this;
    }

}

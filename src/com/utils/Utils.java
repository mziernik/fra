package com.utils;

import com.utils.text.StrWriter;
import com.dev.Dev;
import com.exceptions.FormatException;
import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.intf.runnable.Runnable1;
import com.json.Escape;
import com.lang.LUtil;
import com.mlogger.Log;
import com.utils.collections.TList;
import com.utils.collections.Pair;
import com.utils.reflections.TClass;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.lang.reflect.*;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileLock;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.text.Collator;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

/**
 *
 * @author Miłosz Ziernik
 */
public final class Utils {

    public final static String fullDateFormat = "yyyy-MM-dd HH:mm:ss.SSS";
    public final static Locale PL;
    public final static Locale EN;

    public final static Collator collator = Collator.getInstance(new Locale("pl", "PL"));

    public final static Charset UTF8 = Charset.forName("UTF-8");

    public final static byte[] UTF8_BOM = toByteArray(0xEF, 0xBB, 0xBF);
    public final static Map<byte[], Charset> BOM;

    static {
        Map<byte[], Charset> map = new LinkedHashMap<>();
        map.put(UTF8_BOM, UTF8);
        map.put(toByteArray(0xFF, 0xFE), Charset.forName("UTF-16"));
        map.put(toByteArray(0xFE, 0xFF), Charset.forName("UTF-16"));
        map.put(toByteArray(0xFF, 0xFE, 0x00, 0x00), Charset.forName("UTF-32"));
        map.put(toByteArray(0x00, 0x00, 0xFE, 0xFF), Charset.forName("UTF-32"));
        BOM = Collections.unmodifiableMap(map);

        Locale pl = null;
        Locale en = null;

        for (Locale loc : Locale.getAvailableLocales()) {
            if ("PL".equals(loc.getCountry()) && "pl".equals(loc.getLanguage()))
                pl = loc;

            if ("US".equals(loc.getCountry()) && "en".equals(loc.getLanguage()))
                en = loc;
        }

        PL = pl;
        EN = en;

    }

    /**
     * Pobiera wartość enumeraty na podstawie nazwy ignorując wielkość znaków
     *
     * @param <E>
     * @param enumerate
     * @param key
     * @return
     */
    public static <E extends Enum> E getEnum(Class<E> enumerate, String key) {
        for (E e : enumerate.getEnumConstants())
            if (e.name().equalsIgnoreCase(key))
                return e;
        return null;
    }

    public static <E extends Enum> E getEnumF(Class<E> enumerate, String key) {
        E result = getEnum(enumerate, key);
        if (result == null)
            throw new ServiceException("Constant \"" + key + "\" not found");
        return result;
    }

    public static <T extends Object> T next(T[] arr, int current) {
        return current < arr.length - 1 ? arr[current + 1] : null;
    }

    public static <T extends Object> T prev(T[] arr, int current) {
        return current > 0 ? arr[current - 1] : null;
    }

    /**
     * Sortuje listę ścieżek. Priorytet mają pliki nad katalogami.
     *
     * @param files
     * @return
     */
    public static TList<String> sortFileNames(Iterable<String> files) {
        int max = 0;
        TList< Pair<String, TList<String>>> tmp = new TList<>();
        for (String ss : files) {
            String[] split = ss.split("/");
            if (max < split.length)
                max = split.length;
            tmp.add(new Pair(ss, new TList<>(split)));
        }

        for (Pair<String, TList<String>> lst : tmp)
            while (lst.second.size() < max)
                lst.second.add(0, "\t");

        tmp.sort((Pair<String, TList<String>> o1, Pair<String, TList<String>> o2) -> {
            return o1.second.toString("/").compareTo(o2.second.toString("/"));
        });

        TList<String> result = new TList<>();
        for (Pair<String, TList<String>> pair : tmp)
            result.add(pair.first);
        return result;
    }

    public static String formatDate(Date date) {
        return date == null ? null
                : new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date);
    }

    public static String formatDateMs(Date date) {
        return date == null ? null
                : new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(date);
    }

    public static String randomId() {
        return randomId(10);
    }

    public static String randomId(int len) {
        final String chars = "abcdefghijklmnopqrstuwvxyzABCDEFGHIJKLMNOPQRSTUWVXYZ1234567890";

        Random ran = new Random();
        StrWriter sb = new StrWriter();
        for (int i = 0; i < len; i++)
            sb.append(chars.charAt(ran.nextInt(chars.length() - ((i == 0) ? 10
                    : 0))));
        return sb.toString();
    }

    /**
     * Wyświetla ułamek jako wartość procentową.
     */
    public static String formatPercent(double d, int dec) {
        d *= 100;
        double pow = Math.pow(10, dec);
        return Double.toString(Math.round(d * pow) / pow) + " %";
    }

    public static String formatPercent(double d) {
        return formatPercent(d, ",");
    }

    public static String formatPercent(double d, String separator) {
        return formatFloat(100d * d, separator) + " %";
    }

    public static String toString(Object obj) {
        return toString(obj, false);
    }

    public static String toString(Object obj, boolean checkToStringOverloaded) {
        if (obj == null)
            return null;

        if (obj.getClass().isArray()) {
            StrWriter sb = new StrWriter();
            sb.append("[");

            for (int i = 0; i < Array.getLength(obj); i++) {

                if (sb.length() > 1)
                    sb.append(", ");
                sb.append(toString(Array.get(obj, i), checkToStringOverloaded));

            }
            sb.append("]");
            return sb.toString();
        }

        if (checkToStringOverloaded) {
            // sprawdza czy obiekt ma przeciążoną metodę toString
            String cls = obj.getClass().getName();
            String name = obj.toString();

            if (name.startsWith(cls)
                    && name.length() == cls.length() + 9
                    && name.charAt(cls.length()) == '@')
                Dev.warning("Html Tag",
                        LUtil.OBJECT_WITHOUT_TOSTRING_OVERWRITED.toString(cls));
        }

        return obj.toString();
    }

    public static <T> LinkedList<T> join(Iterable<T>... elements) {
        LinkedList<T> list = new LinkedList<>();
        if (elements != null)
            for (Iterable<T> itr : elements)
                if (itr instanceof Collection)
                    list.addAll((Collection) itr);
                else
                    for (T item : itr)
                        list.add(item);
        return list;
    }

    public static <T extends Object> LinkedList<T> asList(T... elements) {
        LinkedList<T> lst = new LinkedList<>();
        if (elements != null)
            lst.addAll(Arrays.asList(elements));
        return lst;
    }

    public static <T extends Object> LinkedHashSet<T> asSet(T... elements) {
        LinkedHashSet<T> lst = new LinkedHashSet<>();
        if (elements != null)
            lst.addAll(Arrays.asList(elements));
        return lst;
    }

    public static <T> LinkedList<T> asTypedList(Class<? extends T> cls, Iterable<?> list) {
        LinkedList<T> lst = new LinkedList<>();
        for (Object o : list)
            if (o != null && new TClass<>(cls).instanceOf(cls))
                lst.add((T) o);
        return lst;
    }

    public static LinkedList<Integer> asIntList(int... elements) {
        LinkedList<Integer> lst = new LinkedList<>();
        if (elements != null)
            for (int i : elements)
                lst.add(i);
        return lst;
    }

    /**
     * Synchronicznie kopiuje obiekt iterable
     *
     * @param <T>
     * @param iterable
     * @return
     */
    public static <T> Iterable<T> syncItr(Iterable<T> iterable) {
        LinkedList<T> list = new LinkedList<>();
        if (iterable != null)
            synchronized (iterable) {
                if (iterable instanceof Collection)
                    list.addAll((Collection) iterable);
                else
                    for (T t : iterable)
                        list.add(t);

            }
        return list;
    }

    /**
     * Metoda kopiuje kolekcje (lub iterable) do listy
     *
     * @param <T>
     * @param elements
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> LinkedList<T> asList(Iterable<T> elements) {
        LinkedList<T> list = new LinkedList<>();
        if (elements != null)
            if (elements instanceof Collection)
                list.addAll((Collection) elements);
            else
                for (T item : elements)
                    list.add(item);
        return list;
    }

    public static <T> LinkedHashSet<T> asSet(Iterable<T> elements) {
        LinkedHashSet<T> lst = new LinkedHashSet<>();
        lst.addAll(asList(elements));
        return lst;
    }

    public static <T> LinkedList<T> reverse(Iterable<T> elements) {
        LinkedList<T> list = asList(elements);
        Collections.reverse(list);
        return list;
    }

    public static <T> LinkedList<T> reverse(T... elements) {
        LinkedList<T> list = asList(elements);
        Collections.reverse(list);
        return list;
    }

    public static <T> LinkedList<T> shuffle(Iterable<T> elements) {
        LinkedList<T> list = asList(elements);
        Collections.shuffle(list);
        return list;
    }

    public static <T> LinkedList<T> shuffle(T... elements) {
        LinkedList<T> list = asList(elements);
        Collections.shuffle(list);
        return list;
    }

    public static <T extends Comparable> LinkedList<T> sort(Iterable<T> elements) {
        LinkedList<T> list = asList(elements);
        Collections.sort(list);
        return list;
    }

    public static <T extends Comparable> LinkedList<T> sort(T... elements) {
        LinkedList<T> list = asList(elements);
        Collections.sort(list);
        return list;
    }

    public static StackTraceElement getCurrentStackTraceElement(int level) {
        StackTraceElement[] str = Thread.currentThread().getStackTrace();
        level += 2;
        if (level < 0 || level >= str.length)
            return null;
        StackTraceElement ste = str[level];
        return ste;
    }

    public static String getCurrentMethodName(int level) {
        StackTraceElement ste = getCurrentStackTraceElement(level + 1);
        return ste != null
                ? ste.getClassName() + "." + ste.getMethodName() + " (" + ste.getFileName()
                + ":" + ste.getLineNumber() + ")" : null;
    }

    /**
     * Czy obiekty sa identyczne. Porownywane sa kolekcje (Iterable) oraz
     * obiejty dziedziczące po Number
     */
    public static boolean equals(Object o1, Object o2) {
        if (o1 == o2)
            return true;

        if (o1 == null || o2 == null)
            return false;

        if (o1.equals(o2))
            return true;

        if (o1.getClass().isArray() && o2.getClass().isArray()) {

            Object[] a1 = (Object[]) o1;
            Object[] a2 = (Object[]) o2;

            if (a1.length != a2.length)
                return false;

            for (int i = 0; i < a1.length; i++)
                if (!equals(a1[i], a2[i]))
                    return false;

            return true;
        }

        if (o1 instanceof Number && o2 instanceof Number)

            if (((Number) o1).longValue()
                    == ((Number) o2).longValue()
                    || ((Number) o1).doubleValue()
                    == ((Number) o2).doubleValue())
                return true;

        if (o1 instanceof Iterable && o2 instanceof Iterable) {

            Iterator i1 = ((Iterable) o1).iterator();
            Iterator i2 = ((Iterable) o2).iterator();

            if (i1.hasNext() != i2.hasNext())
                return false;

            while (i1.hasNext() && i2.hasNext())
                if (!equals(i1.next(), i2.next()))
                    return false;

            if (i1.hasNext() != i2.hasNext())
                return false;

            return true;
        }

        return false;
    }

    public static <T> boolean contains(T value, T... array) {
        return Is.in(value, array);
    }

    public static InetSocketAddress parseSocketAddress(String s, int defaultPort) {
        if (s == null || s.trim().isEmpty())
            return null;
        final String chars = "01234567890abcdefghijklmnopqrstuwxyz-_.:@";
        try {
            s = s.replace(" ", "").trim();
            for (char c : s.toLowerCase().toCharArray())
                if (chars.indexOf(c) < 0)
                    return null;
            int port = defaultPort;
            if (s.contains(":")) {
                String p = s.substring(s.lastIndexOf(":") + 1);
                port = Integer.parseInt(p.trim());
                s = s.substring(0, s.lastIndexOf(":"));
            }
            return InetSocketAddress.createUnresolved(s.trim(), port);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Zwraca liste z danego zakresu enumeraty
     *
     * @param <E>
     * @param from
     * @param to
     * @return
     */
    public static <E extends Enum> LinkedList<E> enumRange(E from, E to) {

        LinkedList<E> list = new LinkedList<>();
        try {
            Class<? extends Enum> cls = from.getClass();

            E[] values = (E[]) cls.getDeclaredMethod("values").invoke(null);

            boolean ff = false;
            boolean tt = false;
            boolean dir = true;

            for (E item : values) {
                ff |= item == from;
                tt |= item == to;
                if (!ff && !tt)
                    continue;
                dir &= ff;
                list.add(item);

                if (ff && tt)
                    break;
            }

            if (!dir)
                Collections.reverse(list);

        } catch (Exception e) {
            throw new ThrowableException(e);
        }

        return list;
    }

    public static String getProcName(int level) {

        StackTraceElement[] ste = Thread.currentThread().getStackTrace();

        if (level < 0)
            level = 0;
        if (level >= ste.length)
            level = ste.length - 1;
        return ste[level].getMethodName();
    }

    public static byte[] intToBytes(int i) {
        ByteBuffer b = ByteBuffer.allocate(4);
        b.putInt(i);
        return b.array();
    }

    public static Boolean strBool(String s, Boolean defValue) {
        if (s == null)
            return defValue;
        switch (s.toLowerCase()) {
            case "t":
            case "1":
            case "true":
            case "tak":
            case "y":
            case "yes":
            case "on":
                return true;

            case "f":
            case "0":
            case "false":
            case "nie":
            case "n":
            case "no":
            case "off":
                return false;
            default:
                return defValue;
        }
    }

    public static int[] addToArray(int[] array, int val) {

        int size = array == null ? 0 : array.length;
        int[] arr = new int[size + 1];
        for (int i = 0; i < size; i++)
            arr[i] = array[i];
        arr[size] = val;
        return arr;
    }

    public static FormatException checkId(String source, boolean raiseException) {
        return checkId(source, raiseException, null);
    }

    public static FormatException checkId(String source, boolean raiseException, String extraChars) {
        String chars = "0123456789_abcdefghijklmnopqrstuwvxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        if (extraChars != null)
            chars += extraChars;

        if (source == null || source.trim().isEmpty()) {
            FormatException ex = new FormatException(LUtil.VALUE_CANT_BE_EMPTY.toString());
            if (raiseException)
                throw ex;
            return ex;
        }

        for (int i = 0; i < source.length(); i++) {
            char c = source.charAt(i);
            int idx = chars.indexOf(c);

            if (idx < 0 || (i == 0 && idx < 9)) {
                FormatException ex = new FormatException(
                        LUtil.INVALID_CHAR_VALUE_ARG.toString(c, i + 1, source));
                if (raiseException)
                    throw ex;
                return ex;
            }

        }

        return null;
    }

    /**
     * Wymuszone parsowanie integera. Wszystkie nieprawidłowe znaki zostaną
     * zignorowane. Jesli wystąpi znak separatora dziesiętnego, cześć dziesiętna
     * zostanie obcięta
     */
    public static Integer strIntForce(String s, Integer def) {
        if (s == null || s.trim().isEmpty())
            return def;
        Integer result = def;
        try {

            String ss = "";
            for (int i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                if (c == '.' || c == ',')
                    break;
                if (c < '0' || c > '9')
                    continue;
                ss += c;
            }

            result = Integer.parseInt(ss);
        } catch (Exception ex) {
            Log.warning(ex);
        }
        return result;
    }

    public static Integer strInt(String s) {
        return Integer.parseInt(s.replace(" ", ""));
    }

    public static Integer strInt(String s, Integer def) {
        if (s != null)
            try {
                return Integer.parseInt(s.replace(" ", ""));
            } catch (Exception ex) {
            }
        return def;
    }

    public static Long strLong(String s, Long def) {
        if (s != null)
            try {
                return Long.parseLong(s.replace(" ", ""));
            } catch (Exception ex) {
            }
        return def;
    }

    public static Double strDouble(String s, Double def) {
        if (s != null)
            try {
                s = s.trim().replace(" ", "").replace(",", ".");
                if (s.startsWith("."))
                    s = "0" + s;
                return Double.parseDouble(s);
            } catch (Exception ex) {
            }
        return def;
    }

    /*
     public static int strInt(String s) {
     int result = -1;
     try {f
     result = Integer.parseInt(s);
     } catch (Exception ex) {
     }
     return result;
     }
     */
    public static String boolToStr(Boolean b) {
        return b == null ? "" : b ? LUtil.YES.toString() : LUtil.NO.toString();
    }

    public static String formatFloat(double value) {
        return formatFloat(value, ",");
    }

    public static String formatFloat(double value, String separator) {

        if (value > 100)
            value = (double) Math.round(value);
        else if (value > 10)
            value = (double) Math.round(value * 10) / 10;
        else
            value = (double) Math.round(value * 100) / 100;

        int io = (int) Math.round(value);

        if (io == value)
            return formatValue(io, separator);

        return formatValue(value, separator);
    }

    /**
     * Dzieli wartość liczbową na bloki dodając spacje
     */
    public static String formatValue(Number value) {
        return formatValue(value, " ");
    }

    public static String formatValue(String value) {
        return formatValue(value, ".");
    }

    /**
     * Dzieli wartość liczbową na bloki dodając spacje
     */
    public static String formatValue(Number value, String separator) {
        if (value == null)
            return null;

        return formatValue(new DecimalFormat("#.#########################")
                .format(value)
                .replace(",", "."), separator);
    }

    public static String formatValue(String value, String separator) {
        String t = "";
        if (value.contains(".")) {
            t = value.substring(value.indexOf(".") + 1);
            value = value.substring(0, value.indexOf("."));
        }

        String ss = "";
        for (int i = 0; i < value.length(); i++) {
            ss = value.charAt(value.length() - i - 1) + ss;
            if (((i + 1) % 3) == 0 && i < value.length() - 1)
                ss = separator + ss;
        }

        if (!t.isEmpty()) {
            ss += ",";
            for (int i = 0; i < t.length(); i++) {
                ss += t.charAt(i);
                if ((i + 1) % 3 == 0 && i < t.length() - 1)
                    ss += separator;
            }
        }

        return ss.trim();
    }

    public static String formatSize(final long value) {

        double out;
        double val = Math.abs(value);
        String unit;
        String prefix = value < 0 ? "-" : "";

        if (val < 1024) {
            out = val;
            unit = " B";
        } else if (val < 1000 * 1000) {
            out = val / 1024;
            unit = " KB";
        } else if (val < 1000 * 1000 * 1000) {
            out = val / 0x100000;
            unit = " MB";
        } else {
            out = val / 0x40000000;
            unit = " GB";
        }

        if (out > 100)
            out = (double) Math.round(out);
        else if (out > 10)
            out = (double) Math.round(out * 10) / 10;
        else
            out = (double) Math.round(out * 100) / 100;

        int io = (int) Math.round(out);

        if (io == out)
            return prefix + io + unit;

        return prefix + Double.toString(out) + unit;
    }

    public static Object deserialize(byte[] data, String pass) {
        if (data == null)
            return null;
        byte[] buff = data;
        try {
            if (pass != null) {

                SecretKeySpec key = new SecretKeySpec(
                        MessageDigest.getInstance("MD5").digest(pass.getBytes()), "AES");
                Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
                cipher.init(Cipher.DECRYPT_MODE, key);
                buff = cipher.doFinal(buff);

            }
            ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(buff));
            Object obj = in.readObject();
            in.close();
            return obj;
        } catch (Exception ex) {
            return null;
        }
    }

    public static byte[] serialize(Object obj, String pass) {
        if (obj == null)
            return null;
        try {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            ObjectOutput out = new ObjectOutputStream(bout);
            out.writeObject(obj);
            out.close();

            if (pass == null)
                return bout.toByteArray();
            SecretKeySpec key = new SecretKeySpec(
                    MessageDigest.getInstance("MD5").digest(pass.getBytes()), "AES");
            Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
            cipher.init(Cipher.ENCRYPT_MODE, key);
            return cipher.doFinal(bout.toByteArray());
        } catch (Exception ex) {
            return null;
        }
    }

    public static boolean tryStrToInt(String value) {
        try {
            Integer.parseInt(value);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public static String cutLongName(String s, int length, boolean asFileName) {
        if (s == null)
            return s;
        s = s.trim();
        if (s.length() <= length)
            return s;
        return s.substring(0, length - 1).trim() + "… ";
    }

    //***********************************************************************************************
    // --------------------- performacne ---------------------------------
    public final static Map<String, PerformanceStamp> performaceStamps = new TreeMap<>();

    public static String escape(Object object) {

        if (object instanceof TObject)
            object = ((TObject) object).get();

        if (object == null)
            return "null";

        if (object instanceof Number
                || object instanceof Boolean)
            return object.toString();

        if (object.getClass().isArray()
                || object instanceof Collection)
            return Utils.toString(object);

        char[] string = Utils.toString(object).toCharArray();

        StrWriter w = new StrWriter();

        w.append("\"");
        for (char c : string)
            switch (c) {
                case '\\':
                    w.append('\\').append(c);
                    break;
                case '"':
                    w.append("\\\"").append(c);
                    break;
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
                    if (c < 32) {
                        w.append("\\u");
                        String hex = Integer.toHexString(c);
                        w.append("0000", 0, 4 - hex.length());
                        w.append(hex);
                    } else
                        w.append(c);
            }

        w.append("\"");

        return w.toString();
    }

    /**
     * Formatuje string-a podstawiając za parametry %1, %2...%n zaescapowane
     * wartości z args
     *
     * @param string
     * @param args
     * @return
     */
    public static String frmt(String string, Object... args) {

        char[] chars = string.toCharArray();

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < chars.length; i++) {
            char curr = chars[i];
            char prev = i > 0 ? chars[i - 1] : 0;
            char next = i < chars.length - 1 ? chars[i + 1] : 0;
            if (curr == '%' && prev != '%' && next >= '1' && next <= '9') {
                int idx = next - '1';
                if (idx < args.length) {
                    ++i;
                    sb.append(escape(args[idx]));
                    continue;
                }
            }
            sb.append(curr);
        }
        return sb.toString();
    }

    public static class PerformanceStamp {

        public final List<Long> times = new LinkedList<>();
        public Object data;
        public String description;

        public static PerformanceStamp add(String name) {
            synchronized (performaceStamps) {
                PerformanceStamp ps = performaceStamps.get(name);

                if (ps == null) {
                    ps = new PerformanceStamp();
                    performaceStamps.put(name, ps);
                }

                ps.times.add(new Date().getTime());

                if (performaceStamps.size() > 1000)
                    performaceStamps.remove(performaceStamps.keySet().iterator().next());

                return ps;
            }
        }

        private PerformanceStamp() {
        }
    }

    public static String getThreadPriorityName(int priority) {

        if (priority <= 2)
            return LUtil.THE_LOWEST.toString();

        switch (priority) {
            case 3:
            case 4:
                return LUtil.LOW.toString();
            case 5:
                return LUtil.NORMAL.toString();
            case 6:
            case 7:
                return LUtil.HIGH.toString();
            case 8:
            case 9:
                return LUtil.THE_HIGHEST.toString();
            case 10:
                return LUtil.REALTIME.toString();
        }

        return LUtil.UNKNOWN.toString();

    }

    public static String replace(String text, String from, Number to) {
        return replace(text, from, "" + to);
    }

    public static String replace(String text, String from, String to) {
        int pos;
        while ((pos = text.indexOf(from)) >= 0)
            text = text.substring(0, pos) + to
                    + text.substring(pos + from.length(), text.length());
        return text;
    }

    public static <T> T with(T value, Runnable1<T> runnable) {
        runnable.run(value);
        return null;
    }

    /**
     * Zwraca pierwszy argument, który nie jest NULLem
     *
     * @param arguments
     * @return
     */
    public static <T> T coalesce(T... arguments) {
        if (arguments != null)
            for (T o : arguments)
                if (o != null)
                    return o;
        return null;
    }

    public static <T> T random(T... values) {
        if (values == null || values.length == 0)
            return null;
        return values[new Random().nextInt(values.length)];
    }

    public static <T extends Number & Comparable<T>> T min(Iterable<T> values) {
        if (values == null)
            return null;

        T min = null;
        for (T val : values)
            if (min == null || val.compareTo(min) < 0)
                min = val;

        return min;
    }

    public static <T extends Number & Comparable<T>> T max(Iterable<T> values) {
        if (values == null)
            return null;

        T max = null;
        for (T val : values)
            if (max == null || val.compareTo(max) > 0)
                max = val;

        return max;
    }

    /**
     * Metoda weryfikuje czy dana wartość miesci się z zakresie min...max. Jeśli
     * przekracza min lub max, zwracane jest odpowiednio min lub max
     *
     * @param <T>
     * @param value
     * @param min
     * @param max
     * @return
     */
    public static <T extends Number & Comparable<T>> T range(T value, T min, T max) {
        if (value == null)
            return min;
        // min > max
        if (min != null && max != null && min.compareTo(max) > 0) {
            T t = min;
            min = max;
            max = t;
        }
        if (min != null && value.compareTo(min) < 0)
            return min;
        if (max != null && value.compareTo(max) > 0)
            return max;
        return value;
    }

    /**
     * Metoda weryfikuje czy dana wartość miesci się z zakresie min...max. Jeśli
     * przekracza min lub max, zwracana jest wartość domyslna
     *
     * @param <T>
     * @param value
     * @param min
     * @param max
     * @param def
     * @return
     */
    public static <T extends Number & Comparable> T range(T value, T min, T max, T def) {
        if (value == null)
            return null;
        // min > max
        if (min != null && max != null && min.compareTo(max) > 0) {
            T t = min;
            min = max;
            max = t;
        }
        if (min != null && value.compareTo(min) < 0)
            return def;
        if (max != null && value.compareTo(max) > 0)
            return def;
        return value;
    }

    public static String coalesceNonEmpty(Object... arguments) {
        if (arguments != null)
            for (Object o : arguments)
                if (o != null) {
                    String str = Utils.toString(o);
                    if (str != null && !str.isEmpty())
                        return str;
                }
        return null;
    }

    public static String getHostName() {
        try {
            return Utils.coalesce(InetAddress.getLocalHost().getHostName(), "");
        } catch (Exception e) {
            return "";
        }
    }

    public static <K, V> void sortMap(Map<K, V> map, Comparator<Entry<K, V>> comparator) {
        List<Entry<K, V>> list = new LinkedList<>();
        list.addAll(map.entrySet());
        map.clear();
        Collections.sort(list, comparator);
        for (Entry<K, V> en : list)
            map.put(en.getKey(), en.getValue());
    }

    public static void showFileInExplorer(String file) {
        String os = coalesce(System.getProperty("os.name").toLowerCase(), "");
        if (os.contains("win"))
            try {
                new ProcessBuilder("explorer.exe", "/select," + file.replace("/", "\\")).start();
            } catch (IOException ex) {
                Log.warning(ex);
            }
    }

    public static void runBrowser(String url) {

        String os = System.getProperty("os.name").toLowerCase();
        Runtime rt = Runtime.getRuntime();

        try {

            if (os.contains("win"))
                rt.exec("rundll32 url.dll,FileProtocolHandler " + url);
            else if (os.contains("nix") || os.contains("nux")) {
                String[] browsers = {"google-chrome", "epiphany", "firefox",
                    "mozilla", "konqueror", "netscape", "opera", "links", "lynx"};
                StrWriter cmd = new StrWriter();
                for (int i = 0; i < browsers.length; i++)
                    cmd.append(i == 0 ? "" : " || ")
                            .append(browsers[i])
                            .append(" \"")
                            .append(url)
                            .append("\" ");

                rt.exec(new String[]{"sh", "-c", cmd.toString()});

            } else
                return;
        } catch (Exception e) {
            return;
        }
        return;
    }

    public static boolean lockInstance(final String lockFile) {
        try {
            final File file = new File(lockFile);
            file.getParentFile().mkdirs();
            final RandomAccessFile raf = new RandomAccessFile(file, "rw");
            final FileLock fileLock = raf.getChannel().tryLock();
            if (fileLock != null) {
                Runtime.getRuntime().addShutdownHook(new Thread() {
                    @Override
                    public void run() {
                        try {
                            fileLock.release();
                            raf.close();
                            file.delete();
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                });
                raf.setLength(0);
                String name = ManagementFactory.getRuntimeMXBean().getName();
                if (name != null && name.contains("@"))
                    raf.write(name.substring(0, name.indexOf("@")).getBytes());
                return true;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static <T> List<T> find(final Collection<T> collection, final Predicate<? super T> filter) {
        return collection
                .stream()
                .filter(filter)
                .collect(Collectors.toList());
    }

    public static <T> T findFirstSync(final Collection<T> collection, final Predicate<? super T> filter) {
        synchronized (collection) {
            return findFirst(collection, filter);
        }
    }

    public static <T> T findFirst(final Collection<T> collection, final Predicate<? super T> filter) {
        return collection
                .stream()
                .filter(filter)
                .findFirst()
                .orElse(null);
    }

    public static <K, V> Map<K, Pair<V, V>> mapDiff(Map<K, V> map1, Map<K, V> map2) {
        Map<K, V> m1 = new LinkedHashMap<>();
        m1.putAll(map1);

        Map<K, V> m2 = new LinkedHashMap<>();
        m1.putAll(map2);

        Map<K, Pair<V, V>> diff = new LinkedHashMap<>();

        map1.forEach((K key, V v1) -> {
            V v2 = map2.get(key);
            if (Objects.equals(v1, v2)) {
                m1.remove(key);
                m2.remove(key);
                return;
            }
            diff.put(key, new Pair<>(v1, v2));
        });

        m2.forEach((K key, V v2) -> {
            diff.put(key, new Pair<>(null, v2));
        });

        return diff;

    }

    public static void sleep(int milliseconds) {
        try {
            Thread.sleep(milliseconds);

        } catch (InterruptedException ex) {
        }
    }

    @FunctionalInterface
    public static interface Visitor<T> {

        void visit(T element, Visitor<T> visitor);
    }

    public static <T> void visit(T element, Visitor<T> visitor) {
        visitor.visit(element, visitor);
    }

    /**
     * Konwersja int[] do byte[]
     *
     * @param bytes
     * @return
     */
    public static byte[] toByteArray(int... bytes) {
        byte[] result = new byte[bytes.length];
        for (int i = 0; i < bytes.length; i++)
            result[i] = (byte) bytes[i];
        return result;
    }

}

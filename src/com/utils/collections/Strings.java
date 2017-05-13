package com.utils.collections;

import com.exceptions.ThrowableException;
import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.json.Escape;
import java.util.*;
import javax.xml.ws.Holder;

/**
 *
 * @author milosz
 */
public class Strings implements Iterable<String>, Cloneable {

    @FunctionalInterface
    public static interface Escaper<Item extends Object> {

        String escape(Item item);
    }

    //ToDo: Elementy powinny być formatowane w momencie pobierania a nie dodawania
    // ToDo: dodać opcję readOnly 
    // wszystkie elementy doadawać do listy, bez względu na to czy jest wybrane not null, 
    // lub unique. Filtrować w metodzie toString
    private final LinkedList<String> list = new LinkedList<>();
    private String separator = ", ";
    private String prefix;
    private String sufix;
    private boolean nonEmpty;
    private boolean escapeJS;
    private boolean allowNulls;
    private boolean trim;
    private boolean unique = false;
    private boolean caseSensitive = true;
    private final List<Pair<String, String>> replace = new LinkedList<>();
    private StrFormatter formatter;

    public static interface StrFormatter {

        public String format(Object o);
    }

    public Strings() {

    }

    public Strings(String... objects) {
        addAll(objects);
    }

    public Strings(Object... objects) {
        addAll(objects);
    }

    public Strings(Collection<Object> collection) {
        addAll(collection);
    }

    public Strings formatter(StrFormatter formatter) {
        this.formatter = formatter;
        return this;
    }

    public LinkedList<String> astList() {
        LinkedList<String> result = new LinkedList<>();
        result.addAll(list);
        return result;
    }

    public String[] toArray() {
        String[] result = new String[list.size()];
        list.toArray(result);
        return result;
    }

    public Strings replace(String from, Number to) {
        return replace(from, "" + to);
    }

    public Strings separator(String separator) {
        this.separator = separator;
        return this;
    }

    public Strings replace(String from, String to) {
        replace.add(new Pair<>(from, to));
        return this;
    }

    public Strings escapeJS(boolean escapeJS) {
        this.escapeJS = escapeJS;
        return this;
    }

    public final Strings unique(boolean unique) {
        this.unique = unique;
        return this;
    }

    public final Strings caseSensitive(boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
        return this;
    }

    public final Strings prefix(String prefix) {
        this.prefix = prefix;
        return this;
    }

    public final Strings sufix(String sufix) {
        this.sufix = sufix;
        return this;
    }

    public final Strings nonEmpty(boolean nonEmpty) {
        this.nonEmpty = nonEmpty;
        return this;
    }

    public final Strings trim(boolean trim) {
        this.trim = trim;
        return this;
    }

    public final Strings allowNulls(boolean allowNulls) {
        this.allowNulls = allowNulls;
        return this;
    }

    private final Set<Escaper> escapers = new LinkedHashSet<>();

    public Strings escaper(Escaper escaper) {
        escapers.add(escaper);
        return this;
    }

    public Strings addAll(Object... objects) {
        return add(false, objects);
    }

    public Strings addAll(String... objects) {
        return add(false, objects);
    }

    public <T> Strings map(Iterable<T> collection, Escaper<T> mapper) {
        if (collection != null)
            for (T t : collection)
                add(mapper != null ? mapper.escape(t) : Utils.toString(t));
        return this;
    }

    public Strings addAll(Collection<Object> collection) {
        if (collection != null)
            for (Object s : collection)
                add(false, s);
        return this;
    }

    public Strings add(Object object) {
        return add(false, object);
    }

    public Strings insert(Object object) {
        return add(true, object);
    }

    private Strings add(boolean first, Object object) {

        List<Object> items = new LinkedList<>();

        if (object instanceof Strings)
            items.addAll(((Strings) object).list);
        else {

            if (object != null && object.getClass().isArray()) {
                Object[] arr = (Object[]) object;
                for (Object obj : arr)
                    add(first, obj);
                return this;
            }

            if (object != null && object instanceof Iterable) {
                for (Object obj : (Iterable) object)
                    add(first, obj);
                return this;
            }

            items.add(object);
        }

        for (final Object obj : items) {

            final Holder<String> escaped = new Holder<>();
            escapers.forEach((Escaper escaper) -> {
                escaped.value = escaper.escape(object);
            });

            String val = escaped.value;

            if (formatter != null)
                val = formatter.format(obj);
            else
                val = val != null ? val : obj != null ? obj.toString() : null;

            if (!allowNulls && val == null)
                return this;

            if (nonEmpty && (val == null || val.trim().isEmpty()))
                return this;

            if (escapeJS)
                val = new Escape().useQuota(false).toString(obj);

            if (val != null)
                for (Pair<String, String> p : replace)
                    val = Utils.replace(val, p.first, p.second);

            if (unique && contains(val))
                return this;

            if (first)
                list.add(0, val);
            else
                list.add(val);
        }
        return this;
    }

    public String getFormated(int index) {
        if (index >= 0 && index < list.size())
            return (prefix == null ? "" : prefix) + list.get(index) + (sufix == null ? "" : sufix);
        return null;
    }

    public String get(int index) {
        if (index >= 0 && index < list.size())
            return list.get(index);
        return null;
    }

    public String toString(String separator) {
        StrWriter sb = new StrWriter();
        for (String s : list) {
            if (sb.length() > 0)
                sb.append(separator);
            if (prefix != null)
                sb.append(prefix);
            if (trim)
                s = s.trim();
            sb.append(s);
            if (sufix != null)
                sb.append(sufix);
        }
        return sb.toString();
    }

    @Override
    public String toString() {
        return toString(separator);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || (!(obj instanceof Strings)))
            return false;
        return this.list.equals(((Strings) obj).list);
    }

    @Override
    public Iterator<String> iterator() {
        return list.iterator();
    }

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public Strings remove(int index) {
        if (index >= 0 && index < list.size())
            list.remove(index);
        return this;
    }

    public String last() {
        return list.peekLast();
    }

    public String last(boolean remove) {
        return remove ? list.pollLast() : list.peekLast();
    }

    public String first() {
        return list.peekFirst();
    }

    public String first(boolean remove) {
        return remove ? list.pollFirst() : list.peekFirst();
    }

    public Strings remove(String item) {
        list.remove(item);
        return this;
    }

    public boolean has(String text) {
        return contains(text);
    }

    public boolean contains(String text) {
        for (String s : list)
            if ((caseSensitive && s != null && s.equals(text))
                    || (!caseSensitive && s != null && s.equalsIgnoreCase(text)))
                return true;
        return false;

    }

    public int size() {
        return list.size();
    }

    public Strings clear() {
        list.clear();
        return this;
    }

    @Override
    public Strings clone() {
        try {
            return (Strings) super.clone();
        } catch (CloneNotSupportedException ex) {
            throw new ThrowableException(ex);
        }
    }

    public static <T> Strings format(Escaper<T> escaper, T... values) {
        if (values == null || values.length == 0 || escaper == null)
            return new Strings();
        return format(escaper, Arrays.asList(values));
    }

    public static <T> Strings format(Escaper<T> escaper, Iterable<T> values) {
        final Strings result = new Strings();

        if (escaper == null || values == null)
            return result;

        values.forEach((T t) -> {
            String val = escaper.escape(t);
            if (val != null)
                result.add(val);
        });
        return result;
    }

}

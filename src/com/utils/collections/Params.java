package com.utils.collections;

import com.exceptions.ThrowableException;
import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.json.JObject;
import com.utils.collections.Params.Param;
import java.io.*;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.*;
import java.util.Map.Entry;

//ToDo: Dodać implementację VParser-a
public class Params implements Iterable<Param>, Cloneable {

    public final Map<String, Object> extra = new LinkedHashMap<>();

    public class Param implements Comparable<Param>, Cloneable {

        public final Params parent;
        public final String name;
        public final Object value;
        public final boolean escaped;
        public final Map<String, Object> extra = new LinkedHashMap<>();
        public final Object raw;

        private Param(Params parent, boolean escape, String name, Object value) {
            this.parent = parent;
            this.name = name;
            this.value = value == null ? value : escape ? Utils.escape(value) : value;
            this.escaped = escape;
            this.raw = value;
        }

        public void remove() {
            synchronized (parent.list) {
                parent.list.remove(this);
            }
        }

        @Override
        public int compareTo(Param o) {
            return this.name.compareTo(o.name);
        }

        public String getValue() {
            if (value == null)
                return null;
            return value.toString();
        }

        @Override
        public Param clone() throws CloneNotSupportedException {
            return (Param) super.clone();
        }

        public Params getParams() {
            return Params.this;
        }
    }

    protected final TList<Param> list = new TList<>();
    public boolean caseSensitive = true;
    public boolean unique = false;
    public boolean allowNulls = true;

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public void clear() {
        list.clear();
    }

    public Param first() {
        return list.first();
    }

    @Override
    public Params clone() {
        try {
            return (Params) super.clone();
        } catch (CloneNotSupportedException ex) {
            throw new ThrowableException(ex);
        }
    }

    public LinkedList<Object> getValues(String name) {
        LinkedList<Object> list = new LinkedList<>();
        for (Param p : this)
            if (p.name.equals(name))
                list.add(p.value);
        return list;
    }

    public LinkedList<String> getValuesStr(String name) {
        LinkedList<String> list = new LinkedList<>();
        for (Param p : this)
            if (p.name.equals(name))
                list.add(Utils.toString(p.value));
        return list;
    }

    public Map<String, Set<Object>> getMap() {
        Map<String, Set<Object>> map = new HashMap<>();

        for (Param p : this) {
            Set<Object> set = map.get(p.name);
            if (set == null) {
                set = new LinkedHashSet<>();
                map.put(p.name, set);
            }
            set.add(p.value);
        }
        return map;
    }

    public Param add_(boolean escaped, String name, Object value) {
        if (name == null || (!allowNulls && value == null))
            return null;
        Param p = null;
        synchronized (list) {
            if (unique) {
                p = getParam(name);
                if (p != null)
                    p.remove();
            }
            p = new Param(this, escaped, name, value);
            list.add(p);
        }
        return p;
    }

    public Params caseSensitive(boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
        return this;
    }

    public Params extra(String name, Object value) {
        extra.put(name, value);
        return this;
    }

    public Params escape(String name, Object value) {
        add_(true, name, value);
        return this;
    }

    public Params add(String name, Object value) {
        add_(false, name, value);
        return this;
    }

    public Param getParam(String name) {
        synchronized (list) {
            for (Param p : list)
                if ((caseSensitive && p.name.equals(name))
                        || (!caseSensitive && p.name.equalsIgnoreCase(name)))
                    return p;
            return null;
        }
    }

    public LinkedList<Param> getParams(String name) {
        LinkedList<Param> ll = new LinkedList<Param>();
        synchronized (list) {
            for (Param p : list)
                if ((caseSensitive && p.name.equals(name))
                        || (!caseSensitive && p.name.equalsIgnoreCase(name)))
                    ll.add(p);
            return ll;
        }
    }

    public Params remove(String name) {
        synchronized (list) {
            for (Param p : getParams(name))
                p.remove();
        }
        return this;
    }

    public String getF(String name) {
        String result = get(name);
        if (result == null)
            throw new Error("Param \"" + name + "\" not found");
        return result;
    }

    public String get(String name) {
        Param param = getParam(name);
        return param != null ? param.getValue() : null;
    }

    public LinkedList<Param> getGroup(String name) {
        LinkedList<Param> lst = new LinkedList<>();
        synchronized (list) {
            for (Param p : list)
                if ((caseSensitive && p.name.equals(name))
                        || (!caseSensitive && p.name.equalsIgnoreCase(name)))
                    lst.add(p);
        }
        return lst;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Iterator<Param> iterator() {
        return ((List) list.clone()).iterator();
    }

    public Params parseQuery(String query) {
        if (Is.empty(query))
            return this;

        String[] posts = query.split("&");
        for (String s : posts) {
            String s1;
            String s2;
            if (s.contains("=")) {
                s1 = s.substring(0, s.indexOf("="));
                s2 = s.substring(s.indexOf("=") + 1, s.length());
                try {
                    s1 = URLDecoder.decode(s1, "UTF-8");
                    s2 = URLDecoder.decode(s2, "UTF-8");
                } catch (UnsupportedEncodingException ex) {
                    throw new ThrowableException(ex);
                }

            } else {
                try {
                    s1 = URLDecoder.decode(s, "UTF-8");
                } catch (UnsupportedEncodingException ex) {
                    throw new ThrowableException(ex);
                }
                s2 = null;
            }
            add(s1, s2);
        }
        return this;
    }

    @Override
    public String toString() {
        StrWriter sb = new StrWriter();
        synchronized (list) {
            for (Param p : list) {
                if (sb.length() > 0)
                    sb.append(", ");
                sb.append(p.name).append(" = ");
                if (p.value == null)
                    sb.append("null");
                else if (p.value instanceof String)
                    sb.append("\"")
                            .append(Utils.cutLongName(p.value.toString(), 100, false))
                            .append("\"");
                else
                    sb.append(Utils.cutLongName(p.value.toString(), 100, false));
            }
        }
        return sb.toString();
    }

    public JObject toJson() {

        JObject json = new JObject();

        for (Entry<String, Set<Object>> en : getMap().entrySet()) {
            String key = en.getKey();
            Set<Object> value = en.getValue();

            if (value.size() > 1)
                json.arrayC(key).addAll(value);
            else if (value.size() == 1)
                json.put(key, value.iterator().next());
            else
                json.put(key, null);
        }

        return json;

    }

    public String toURI() {
        StrWriter sb = new StrWriter();

        synchronized (list) {
            for (Param p : list) {
                if (sb.length() > 0)
                    sb.append("&");
                try {
                    sb.append(URLEncoder.encode(p.name, "UTF-8")
                            .replaceAll("\\+", "%20"));
                } catch (UnsupportedEncodingException ex) {
                }
                String val = p.getValue();
                if (val == null)
                    continue;
                try {
                    sb.append("=").append(URLEncoder.encode(val, "UTF-8")
                            .replaceAll("\\+", "%20"));
                } catch (UnsupportedEncodingException ex) {
                }
            }
        }
        return sb.toString();
    }

    public void addAll(Params other) {
        list.addAll(other.list);
    }

    public void addAll(Collection<Param> other) {
        list.addAll(list);
    }

    public Params addAll(Map<String, List<String>> map) {
        if (map == null)
            return this;
        return addAllEntries(map.entrySet());
    }

    public Params addAllEntries(Collection<Entry<String, List<String>>> entries) {
        if (entries == null)
            return this;

        for (Entry<String, List<String>> en : entries)
            if (en.getKey() != null && en.getValue() != null)
                for (String s : en.getValue())
                    add(en.getKey(), s);

        return this;
    }

}

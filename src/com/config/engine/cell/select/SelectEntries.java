package com.config.engine.cell.select;

import com.config.engine.ConfigException;
import com.config.engine.cell.select.SelectEntries.SelectEntry;
import com.utils.Utils;
import com.utils.Is;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class SelectEntries<T> implements Iterable<SelectEntry<T>> {

    private boolean caseSensitive = false;

    public final Map<String, SelectEntry<T>> map = new LinkedHashMap<>();

    public SelectEntries() {
    }

    public SelectEntries(Map<String, T> map) {
        if (map != null)
            for (Entry<String, T> en : map.entrySet())
                put(en.getKey(), en.getValue());
    }

    public boolean caseSensitive() {
        return caseSensitive;
    }

    public SelectEntries<T> caseSensitive(boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
        return this;
    }

    @Override
    public Iterator<SelectEntry<T>> iterator() {
        return map.values().iterator();
    }

    public T getF(String key) {

        SelectEntry<T> entry = caseSensitive
                ? map.get(key)
                : Utils.findFirst(map.values(), t -> t.key.equalsIgnoreCase(key));

        if (entry == null)
            throw new ConfigException(String.format("Config entry \"%s\" not found", key));
        return entry.value;
    }

    public SelectEntries<T> put(String key, String name, T value) {
        map.put(key, new SelectEntry<>(key, name, value));
        return this;
    }

    public SelectEntries<T> put(String key, T value) {
        map.put(key, new SelectEntry<>(key, value));
        return this;
    }

    public static class SelectEntry<T> {

        public final String key;
        public final String name;
        public final T value;

        public SelectEntry(String key, String name, T value) {
            this.key = key;
            this.name = name;
            this.value = value;
        }

        public SelectEntry(String key, T value) {
            this.key = key;
            this.name = Utils.toString(value);
            this.value = value;
        }

        @Override
        public String toString() {
            return key + " \"" + name + "\"";
        }

    }
}

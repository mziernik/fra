package com.utils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

public class Counter<T> extends HashMap<T, Integer>
        implements Iterable<Entry<T, Integer>> {

    @Override
    public Integer get(Object key) {
        Integer val = super.get(key);
        if (val == null)
            val = 0;
        return val;
    }

    public Integer inc(T object) {
        synchronized (this) {
            Integer cnt = get(object) + 1;
            put(object, cnt);
            return cnt;
        }
    }

    public void incAll(Counter<T> counter) {
        synchronized (this) {
            for (Entry<T, Integer> en : counter.entrySet())
                put(en.getKey(), get(en.getKey()) + en.getValue());
        }
    }

    @Override
    public Iterator<Entry<T, Integer>> iterator() {
        List<Entry<T, Integer>> list;
        synchronized (this) {
            list = new LinkedList<>(entrySet());
        }
        return list.iterator();
    }

}

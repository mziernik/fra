package com.utils.collections;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map.Entry;

public class Pair<First, Second> implements Iterable<Object> {

    public First first;
    public Second second;

    public Pair(First first, Second second) {
        this.first = first;
        this.second = second;
    }

    public Pair(Entry<First, Second> entry) {
        this.first = entry.getKey();
        this.second = entry.getValue();
    }

    public ArrayList<Object> values() {
        ArrayList<Object> list = new ArrayList<>();
        list.add(first);
        list.add(second);
        return list;
    }

    @Override
    public int hashCode() {
        return 31 * hashcode(first) + hashcode(second);
    }

    private static int hashcode(Object o) {
        return o == null ? 0 : o.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Pair))
            return false;
        if (this == obj)
            return true;
        return equal(first, ((Pair) obj).first)
                && equal(second, ((Pair) obj).second);
    }

    private boolean equal(Object o1, Object o2) {
        return o1 == null ? o2 == null : (o1 == o2 || o1.equals(o2));
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ')';
    }

    @Override
    public Iterator<Object> iterator() {
        return values().iterator();
    }
}

package com.utils.collections;

import java.util.*;

/**
 * Miłosz Ziernik 2013/11/09
 */
public class Quad<First, Second, Third, Fourth> implements Iterable<Object> {

    public First first;
    public Second second;
    public Third third;
    public Fourth fourth;

    public Quad(First first, Second second, Third third, Fourth fourth) {
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
    }

    public ArrayList<Object> values() {
        ArrayList<Object> list = new ArrayList<>();
        list.add(first);
        list.add(second);
        list.add(third);
        list.add(fourth);
        return list;
    }

    @Override
    public int hashCode() {
        return 31 * hashcode(first) + hashcode(second) + hashcode(third) + hashcode(fourth);
    }

    private static int hashcode(Object o) {
        return o == null ? 0 : o.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Quad))
            return false;
        if (this == obj)
            return true;
        return equal(first, ((Quad) obj).first)
                && equal(second, ((Quad) obj).second)
                && equal(third, ((Quad) obj).third)
                && equal(fourth, ((Quad) obj).fourth);
    }

    private boolean equal(Object o1, Object o2) {
        return o1 == null ? o2 == null : (o1 == o2 || o1.equals(o2));
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ", " + third + ", " + fourth + ')';
    }

    @Override
    public Iterator<Object> iterator() {
        return values().iterator();
    }
}

package com.utils.collections;

import com.utils.Utils;
import com.utils.Is;
import java.util.*;

/**
 *
 * @author milosz
 * @param <First>
 * @param <Second>
 */
public class Pairs<First, Second> implements Iterable<Pair<First, Second>> {

    private final LinkedList<Pair<First, Second>> list = new LinkedList<>();

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();

        for (Pair<First, Second> p : list)
            sb.append(sb.length() > 0 ? "\n" : "")
                    .append(p.first != null ? p.first.toString() : "null")
                    .append(", ")
                    .append(p.second != null ? p.second.toString() : "null");
        return sb.toString();
    }

    public List<Pair<First, Second>> asList() {
        LinkedList<Pair<First, Second>> result = new LinkedList<>();
        result.addAll(list);
        return result;
    }

    public LinkedHashMap<First, Second> asMap() {
        LinkedHashMap<First, Second> map = new LinkedHashMap<>();
        for (Pair<First, Second> pair : list)
            map.put(pair.first, pair.second);
        return map;
    }

    public List<First> firstList() {
        List<First> lst = new LinkedList<>();
        for (Pair<First, Second> pair : list)
            lst.add(pair.first);
        return lst;
    }

    public List<Second> secondList() {
        List<Second> lst = new LinkedList<>();
        for (Pair<First, Second> pair : list)
            lst.add(pair.second);
        return lst;
    }

    public void sort(Comparator<Pair<First, Second>> comparator) {
        Collections.sort(list, comparator);
    }

    public Pair<First, Second> first() {
        return list.peekFirst();
    }

    public Pair<First, Second> last() {
        return list.peekLast();
    }

    public void sortFirst() {
        sort((Pair<First, Second> o1, Pair<First, Second> o2) -> {
            if (o1 == null || o2 == null)
                return 0;
            return Utils.collator.compare(o1.first, o2.first);
        });
    }

    public void sortFirst(final Comparator<First> comparator) {
        sort((Pair<First, Second> o1, Pair<First, Second> o2) -> comparator.compare(o1 != null ? o1.first : null,
                o2.first != null ? o2.first : null));
    }

    public void sortSecond() {
        sort((Pair<First, Second> o1, Pair<First, Second> o2) -> {
            if (o1 == null || o2 == null)
                return 0;
            return Utils.collator.compare(o1.second, o2.second);
        });
    }

    public void sortSecond(final Comparator<Second> comparator) {
        sort((Pair<First, Second> o1, Pair<First, Second> o2) -> comparator.compare(o1 != null ? o1.second : null,
                o2.second != null ? o2.second : null));
    }

    public Pairs<First, Second> add(First first, Second second) {
        Pair<First, Second> pair = new Pair<>(first, second);
        list.add(pair);
        return this;
    }

    public void add(Pair<First, Second> pair) {
        list.add(pair);
    }

    public void addAll(Iterable<Pair<First, Second>> pairs) {
        list.addAll(Utils.asList(pairs));
    }

    public Pair insert(First first, Second second) {
        Pair<First, Second> pair = new Pair<>(first, second);
        list.add(0, pair);
        return pair;
    }

    public boolean remove(Pair<First, Second> pair) {
        return list.remove(pair);
    }

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public int size() {
        return list.size();
    }

    @Override
    public Iterator<Pair<First, Second>> iterator() {
        return list.iterator();
    }

    public LinkedList<Second> get(First first) {
        LinkedList<Second> lst = new LinkedList<>();
        for (Pair<First, Second> p : this.list)
            if (p.first != null && p.first.equals(first))
                lst.add(p.second);
        return lst;
    }

    public void clear() {
        list.clear();
    }

    public boolean contains(First first, Second second) {
        for (Pair<First, Second> pair : list)
            if (Objects.equals(first, pair.first)
                    && Objects.equals(second, pair.second))
                return true;
        return false;
    }

    public boolean containsFist(First first) {
        for (Pair<First, Second> pair : list)
            if (Objects.equals(first, pair.first))
                return true;
        return false;
    }

    public boolean containsSecond(Second second) {
        for (Pair<First, Second> pair : list)
            if (Objects.equals(second, pair.second))
                return true;
        return false;
    }

}

package com.filter;

import com.json.JArray;
import com.lang.LFilter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Błażej Palmąka 2017/01/16
 */
public class Filters<TFilter extends AbstractFilter<TFilter>> implements Iterable<TFilter> {

    public final List<TFilter> filters = new LinkedList<>();

    public static enum SortBy {
        KEY, CAPTION, ADD
    }

    public TFilter add(TFilter filter) {
        filters.add(filter);
        return filter;
    }

    public boolean remove(TFilter filter) {
        return filters.remove(filter);
    }

    public void clear() {
        filters.clear();
    }

    public Optional<TFilter> get(String key) {
        return filters.stream()
                .filter((TFilter f) -> f.key.equals(key))
                .findFirst();
    }

    public TFilter getF(String key) {
        return get(key)
                .orElseThrow(() -> new Error(LFilter.FILTER_NOT_FOUND.toString(key)));
    }

    public Stream<TFilter> stream() {
        return filters.stream();
    }

    public void sort(SortBy sort) {
        filters.sort((TFilter o1, TFilter o2) -> {
            switch (sort) {
                case CAPTION:
                    return o1.caption.toString().compareTo(o2.caption.toString());
                case KEY:
                    return o1.key.compareTo(o2.key);
                default:
                    return o1.order.compareTo(o2.order);
            }
        });
    }

    public void sort(Comparator<TFilter> comparator) {
        filters.sort(comparator);
    }

    public JArray toJson() {
        return new JArray().addAll(this.stream()
                .map(TFilter::toJson)
                .collect(Collectors.toList()));
    }

    @Override
    public Iterator<TFilter> iterator() {
        return filters.iterator();
    }
}

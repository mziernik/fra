package com.filter.data;

import com.filter.FCondition;
import com.filter.AbstractFilter;
import com.filter.Filters;
import com.lang.LFilter;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

/**
 * @author Błażej Palmąka 2017/01/25
 */
public class FilterGroupData<TFilter extends AbstractFilter<TFilter>> implements Iterable<FilterData<TFilter>> {

    final Filters<TFilter> filters;
    final List<FilterData<TFilter>> groupValues = new LinkedList<>();

    FilterGroupData(Filters<TFilter> filters) {
        this.filters = filters;
    }

    public void clear() {
        this.groupValues.clear();
    }

    public void removeFilterValues(TFilter filter) {
        FilterData<TFilter> fData = get(filter);

        if (fData != null)
            groupValues.remove(fData);
    }

    public FilterGroupData<TFilter> addFilterValue(TFilter filter, FCondition cond, String value) {
        Objects.requireNonNull(filter, LFilter.FILTER_NOT_SPECIFIED.toString());
        Objects.requireNonNull(cond, LFilter.CONDITION_NOT_SPECIFIED.toString());

        if (!filter.supportedCondition(cond))
            throw new Error(LFilter.FILTER_DOESNT_SUPPORT_CONDITION.toString(filter.caption.toString(), cond.name()));

        getD(filter)
                .add(cond, value);

        return this;
    }

    FilterData<TFilter> get(TFilter filter) {
        return groupValues.stream()
                .filter((FilterData t) -> t.filter == filter)
                .findFirst()
                .orElse(null);
    }

    FilterData<TFilter> getD(TFilter filter) {
        FilterData fData = get(filter);

        if (fData == null) {
            fData = new FilterData<TFilter>(filter);
            groupValues.add(fData);
        }

        return fData;
    }

    boolean isEmpty() {
        return groupValues.isEmpty();
    }

    @Override
    public Iterator<FilterData<TFilter>> iterator() {
        return groupValues.iterator();
    }
}

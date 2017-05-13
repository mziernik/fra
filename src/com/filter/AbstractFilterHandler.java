package com.filter;

import com.filter.data.FilterGroupData;
import com.filter.data.FilterGroupsData;
import com.filter.Filters.SortBy;
import com.json.JArray;
import com.json.JObject;
import com.utils.collections.TList;
import java.util.Comparator;
import java.util.List;

/**
 * @author Błażej Palmąka 2017/01/16
 */
public abstract class AbstractFilterHandler<TId, TFilter extends AbstractFilter<TFilter>, TOutputResult> {

    protected final Filters<TFilter> filters = new Filters<>();
    protected final FilterGroupsData<TFilter> filtersData = new FilterGroupsData<>(filters);

    public int resultLimit = 5000;
    public final TList<TId> resultsIds = new TList<>();

    // Metody filtrów --------------------------------------
    public TFilter addFilter(TFilter filter) {
        return filters.add(filter);
    }

    public boolean removeFilter(TFilter filter) {
        filtersData.removeFilterValues(filter);
        return filters.remove(filter);
    }

    public void clearFilters() {
        filters.clear();
    }

    public void sortFilters(SortBy sort) {
        filters.sort(sort);
    }

    public void sortFilters(Comparator<TFilter> comparator) {
        filters.sort(comparator);
    }

    // Metody danych ---------------------------------------
    public FilterGroupData<TFilter> addNewDataGroup() {
        return filtersData.addNewGroup();
    }

    public boolean removeDataGroup(FilterGroupData<TFilter> dataGroup) {
        return filtersData.remove(dataGroup);
    }

    public void clearData() {
        filtersData.clear();
    }

    //------------------------------------------------------
    public void filter(JArray filters, JObject params) throws Exception {
        filtersData.parse(filters);
        resultsIds.clear();
        doFilter(params);
    }

    protected abstract void doFilter(JObject params) throws Exception;

    public abstract TOutputResult getData(List<TId> dataIds) throws Exception;

    public abstract TOutputResult getData(int offset, int entries) throws Exception;

    public abstract TOutputResult getDataPage(int page, int entries) throws Exception;

    public abstract JObject getDetails(TId id) throws Exception;

    public JObject toJson() {
        return new JObject()
                .add("filters", filters.toJson())
                .add("activeFilters", filtersData.toJson());
    }
}

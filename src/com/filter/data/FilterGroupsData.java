package com.filter.data;

import com.filter.FCondition;
import com.filter.AbstractFilter;
import com.filter.Filters;
import com.json.JArray;
import com.json.JObject;
import com.json.JValue;
import com.lang.LFilter;
import java.util.*;

/**
 * @author Błażej Palmąka 2017/01/24
 */
public class FilterGroupsData<TFilter extends AbstractFilter<TFilter>> implements Iterable<FilterGroupData<TFilter>> {

    final Filters<TFilter> filters;
    final List<FilterGroupData<TFilter>> groupsData = new LinkedList<>();

    public FilterGroupsData(Filters<TFilter> filters) {
        this.filters = filters;
    }

    public void clear() {
        groupsData.clear();
    }

    public FilterGroupData<TFilter> addNewGroup() {
        FilterGroupData<TFilter> groupData = new FilterGroupData<>(filters);
        groupsData.add(groupData);
        return groupData;
    }

    public boolean remove(FilterGroupData<TFilter> groupData) {
        return groupsData.remove(groupData);
    }

    public void removeFilterValues(TFilter filter) {
        for (FilterGroupData<TFilter> groupData : this)
            groupData.removeFilterValues(filter);
    }

    public Optional<FilterGroupData<TFilter>> get(int index) {
        if (index >= 0 && index < groupsData.size())
            return Optional.of(groupsData.get(index));
        return Optional.empty();
    }

    public void parse(JArray values) throws Exception {
        clear();

        if (values == null || values.isEmpty())
            return;

        try {
            // {on:true, filters: [["users", "equal", "jan"], ...]}
            for (JObject grp : values.getObjects()) {
                if (!grp.getBool("on"))
                    continue;

                FilterGroupData<TFilter> groupData = addNewGroup();

                // ["users", "equal", "jan"]
                for (JArray fltrElem : grp.arrayF("filters").getArrays()) {
                    String key = fltrElem.element(0).asValue().asString();
                    String cond = fltrElem.element(1).asValue().asString().toUpperCase();
                    String val = fltrElem.element(2, new JValue("")).asValue().asString();

                    groupData.addFilterValue(
                            filters.getF(key),
                            FCondition.getByNameF(cond),
                            val);
                }

                if (groupData.isEmpty())
                    remove(groupData);
            }

        } catch (Exception ex) {
            throw new Exception(LFilter.PARSE_FILTERS_ERROR.toString(), ex);
        }
    }

    public JArray toJson() {
        JArray json = new JArray();

        for (FilterGroupData<TFilter> groupData : this)
            for (FilterData<TFilter> fData : groupData) {
                if (fData.isEmpty())
                    continue;

                for (FCondition cond : fData.keySet())
                    for (String value : fData.get(cond))
                        json.add(new JObject()
                                .add("key", fData.filter.key)
                                .add("cond", cond.name().toLowerCase())
                                .add("value", value)
                        );
            }

        return json;
    }

    public boolean isEmpty() {
        return groupsData.isEmpty();
    }

    public int indexOf(FilterGroupData<TFilter> groupData) {
        return groupsData.indexOf(groupData);
    }

    @Override
    public Iterator<FilterGroupData<TFilter>> iterator() {
        return groupsData.iterator();
    }
}

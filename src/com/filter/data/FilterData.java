package com.filter.data;

import com.filter.FCondition;
import com.filter.AbstractFilter;
import com.utils.collections.MapList;

/**
 * @author Błażej Palmąka 2017/01/25
 */
public class FilterData<TFilter extends AbstractFilter<TFilter>> extends MapList<FCondition, String> {

    public final TFilter filter;

    FilterData(TFilter filter) {
        this.filter = filter;
    }
}

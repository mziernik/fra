package com.service.events.filter;

import com.filter.AbstractFilter;
import com.lang.core.LStr;

/**
 * @author Błażej Palmąka 2017/01/26
 */
public class EventFilter extends AbstractFilter<EventFilter> {

    public static enum TableSearchType {
        ANY, ATTRIB, FOREIGN_KEY, CUSTOM, EVENT
    }

    public final TableSearchType tblSearchType;

    public EventFilter(String key, LStr caption, TableSearchType tblSearchType) {
        super(key, caption);
        this.tblSearchType = tblSearchType;
    }
}

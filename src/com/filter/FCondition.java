package com.filter;

import static com.filter.FFilterType.*;
import static com.lang.LFilter.*;
import com.exceptions.ServiceException;
import com.lang.LFilter;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Błażej Palmąka 2015/12/30
 */
public enum FCondition {
    EQUAL(COND__EQUAL, TEXT, NUMBER, BOOLEAN, ENUM, DATE, TIME, DATETIME),
    NOT_EQUAL(COND__NOT_EQUAL, TEXT, NUMBER, BOOLEAN, ENUM, DATE, TIME, DATETIME),
    START_WITH(COND__STARTS_WITH, TEXT),
    ENDS_WITH(COND__ENDS_WITH, TEXT),
    CONTAINS(COND__CONTAINS, TEXT, TEXT_ARRAY),
    NOT_CONTAIN(COND__NOT_CONTAIN, TEXT, TEXT_ARRAY),
    SIMILAR_20(COND__SIMILAR_20, TEXT),
    SIMILAR_50(COND__SIMILAR_50, TEXT),
    SIMILAR_80(COND__SIMILAR_80, TEXT),
    EMPTY(COND__IS_EMPTY, TEXT, NUMBER, BOOLEAN, ENUM, DATE, TIME, DATETIME, TEXT_ARRAY),
    NOT_EMPTY(COND__IS_NOT_EMPTY, TEXT, NUMBER, BOOLEAN, ENUM, DATE, TIME, DATETIME, TEXT_ARRAY),
    GREATER_OR_EQUAL(COND__GREATER_OR_EQUAL, NUMBER, DATE, TIME, DATETIME),
    LESS_OR_EQUAL(COND__LESS_OR_EQUAL, NUMBER, DATE, TIME, DATETIME),
    GREATER(COND__GREATER, NUMBER, DATE, TIME, DATETIME),
    LESS(COND__LESS, NUMBER, DATE, TIME, DATETIME),
    OLDER_THAN_MINUTES(COND__OLDER_THAN_MINUTES, NUMBER),
    OLDER_THAN_HOURS(COND__OLDER_THAN_HOURS, NUMBER),
    OLDER_THAN_DAYS(COND__OLDER_THAN_DAYS, NUMBER),
    OLDER_THAN_MONTHS(COND__OLDER_THAN_MONTHS, NUMBER),
    OLDER_THAN_YEARS(COND__OLDER_THAN_YEARS, NUMBER),
    YOUNGER_THAN_MINUTES(COND__YOUNGER_THAN_MINUTES, NUMBER),
    YOUNGER_THAN_HOURS(COND__YOUNGER_THAN_HOURS, NUMBER),
    YOUNGER_THAN_DAYS(COND__YOUNGER_THAN_DAYS, NUMBER),
    YOUNGER_THAN_MONTHS(COND__YOUNGER_THAN_MONTHS, NUMBER),
    YOUNGER_THAN_YEARS(COND__YOUNGER_THAN_YEARS, NUMBER);

    private final LFilter caption;           // Słowny zapis warunku
    private final List<FFilterType> fTypes;  // Typy filtrów których dotyczy warunek

    private FCondition(LFilter caption, FFilterType... fieldTypes) {
        this.caption = caption;
        fTypes = Arrays.asList(fieldTypes);
    }

    public String getCaption() {
        return caption.toString();
    }

    public static FCondition getByName(String name) {
        for (FCondition cond : values())
            if (cond.name().equals(name))
                return cond;
        return null;
    }

    public static FCondition getByNameF(String name) {
        FCondition cond = getByName(name);

        if (cond != null)
            return cond;

        throw new ServiceException(LFilter.UNSUPPORTED_FILTER_CONDITION.toString(name));

    }

    public static List<FCondition> getForType(FFilterType filterType) {
        List<FCondition> result = new LinkedList<>();

        for (FCondition cond : values())
            if (cond.supportsType(filterType))
                result.add(cond);

        return result;
    }

    private boolean supportsType(FFilterType filterType) {
        return fTypes.contains(filterType);
    }

    public static boolean isOlderOrYoungerCond(FCondition filterCond) {
        switch (filterCond) {
            case OLDER_THAN_MINUTES:
            case OLDER_THAN_HOURS:
            case OLDER_THAN_DAYS:
            case OLDER_THAN_MONTHS:
            case OLDER_THAN_YEARS:
            case YOUNGER_THAN_MINUTES:
            case YOUNGER_THAN_HOURS:
            case YOUNGER_THAN_DAYS:
            case YOUNGER_THAN_MONTHS:
            case YOUNGER_THAN_YEARS:
                return true;
            default:
                return false;
        }
    }
}

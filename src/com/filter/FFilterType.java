package com.filter;

/**
 * @author Błażej Palmąka 2015/12/30
 */
public enum FFilterType {
    TEXT,
    NUMBER,
    BOOLEAN,
    ENUM,
    DATE,
    TIME,
    DATETIME,
    TEXT_ARRAY;

    public boolean oneOf(FFilterType... enumValues) {
        for (FFilterType enumVal : enumValues)
            if (this == enumVal)
                return true;

        return false;
    }
}

package com.filter;

import static com.filter.FCondition.*;
import static com.filter.FFilterType.*;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import java.util.List;

/**
 * @author Błażej Palmąka 2016/02/19
 */
public class SqlConditionBuilder {

    static boolean equalToIlikeForTextTypes = true;

    public static String buildCondition(String tblName, FFilterType fType, FCondition fCond, List<String> condValues) {
        FCondition cond = fCond;
        Strings values = getFormatedValues(condValues, fType, fCond);
        String tableName = castTableToTextIfNeeded(tblName, fType, fCond, values.size());
        boolean multiValues = values.size() > 1;

        if (values.size() < 1)
            return null;

        String val = multiValues ? "ARRAY[" + values.toString() + "]" : values.getFormated(0);

        // Zamiana warunku 'EQUAL/NOT_EQUAL' na 'ILIKE/NOT_ILIKE' jeśli zezwolone
        if (equalToIlikeForTextTypes && (fCond == EQUAL || fCond == NOT_EQUAL) && (fType == TEXT || fType == TEXT_ARRAY)) {
            if (fCond == EQUAL)
                cond = CONTAINS;
            if (fCond == NOT_EQUAL)
                cond = NOT_CONTAIN;
        }

        switch (cond) {
            case CONTAINS:
            case START_WITH:
            case ENDS_WITH:
                return tableName + "::text ILIKE " + (multiValues ? "ANY(" + val + ")" : val);
            case NOT_CONTAIN:
                return tableName + "::text NOT ILIKE " + (multiValues ? "ALL(" + val + ")" : val);
            case EQUAL:
                return tableName + " = " + (multiValues ? "ANY(" + val + ")" : val);
            case NOT_EQUAL:
                return tableName + " != " + (multiValues ? "ALL(" + val + ")" : val);
            case GREATER:
                return tableName + " > " + (multiValues ? "ANY(" + val + ")" : val);
            case GREATER_OR_EQUAL:
                return tableName + " >= " + (multiValues ? "ANY(" + val + ")" : val);
            case LESS:
                return tableName + " < " + (multiValues ? "ANY(" + val + ")" : val);
            case LESS_OR_EQUAL:
                return tableName + " <= " + (multiValues ? "ANY(" + val + ")" : val);
            case EMPTY:
                return tableName + " IS NULL";
            case NOT_EMPTY:
                return tableName + " IS NOT NULL";
            case SIMILAR_20:
                return "similarity(" + tableName + ", " + val + ") > 0.2";
            case SIMILAR_50:
                return "similarity(" + tableName + ", " + val + ") > 0.5";
            case SIMILAR_80:
                return "similarity(" + tableName + ", " + val + ") > 0.8";
            case OLDER_THAN_MINUTES:
            case OLDER_THAN_HOURS:
            case OLDER_THAN_DAYS:
            case OLDER_THAN_MONTHS:
            case OLDER_THAN_YEARS:
                return tableName + " < " + (multiValues ? "ANY(" + val + ")" : val);
            case YOUNGER_THAN_MINUTES:
            case YOUNGER_THAN_HOURS:
            case YOUNGER_THAN_DAYS:
            case YOUNGER_THAN_MONTHS:
            case YOUNGER_THAN_YEARS:
                return tableName + " > " + (multiValues ? "ANY(" + val + ")" : val);
        }

        return null;
    }

    static Strings getFormatedValues(List<String> values, FFilterType type, FCondition cond) {
        Strings strValues = new Strings();

        if (FCondition.isOlderOrYoungerCond(cond)) {
            for (String val : values)
                if (val != null && !val.equals("")) {
                    int num = -Integer.parseInt(val);

                    switch (cond) {
                        case OLDER_THAN_MINUTES:
                        case YOUNGER_THAN_MINUTES:
                            strValues.add(new TDate().addMinutes(num));
                            break;
                        case OLDER_THAN_HOURS:
                        case YOUNGER_THAN_HOURS:
                            strValues.add(new TDate().addHours(num));
                            break;
                        case OLDER_THAN_DAYS:
                        case YOUNGER_THAN_DAYS:
                            strValues.add(new TDate().addDays(num));
                            break;
                        case OLDER_THAN_MONTHS:
                        case YOUNGER_THAN_MONTHS:
                            strValues.add(new TDate().addMonths(num));
                            break;
                        case OLDER_THAN_YEARS:
                        case YOUNGER_THAN_YEARS:
                            strValues.add(new TDate().addYears(num));
                            break;
                    }
                }
        } else
            for (String val : values)
                if (val != null && !val.equals(""))
                    strValues.add(val);

        // Dodanie jednej pustej wartości dla warunków które je obsługują
        if (strValues.isEmpty() && (type != NUMBER && type != BOOLEAN)
                && ((cond == EMPTY || cond == NOT_EMPTY) || (!equalToIlikeForTextTypes && (cond == EQUAL || cond == NOT_EQUAL))))
            strValues.add("");

        if (strValues.isEmpty() && (cond == EMPTY || cond == NOT_EMPTY))
            strValues.add("");

        // Zostawienie tylko 1 wartości dla warunków które nie obsługują więcej
        if (strValues.size() > 1 && (cond == SIMILAR_20 || cond == SIMILAR_50 || cond == SIMILAR_80)) {
            String value = strValues.get(0);
            strValues.clear();
            strValues.add(value);
        }

        // Dodanie odpowiednich prefiksów i surfiksów
        if (type != NUMBER && type != BOOLEAN)
            strValues.prefix("'").sufix("'");
        if (cond == CONTAINS || cond == NOT_CONTAIN)
            strValues.prefix("'%").sufix("%'");
        if (cond == START_WITH)
            strValues.prefix("'%").sufix("'");
        if (cond == ENDS_WITH)
            strValues.prefix("'").sufix("%'");

        return strValues;
    }

    static String castTableToTextIfNeeded(String tblName, FFilterType type, FCondition cond, int valuesSize) {
        if (cond == EMPTY || cond == NOT_EMPTY)
            return tblName;

        if (type == FFilterType.TEXT_ARRAY)
            return tblName + "::text";

        if (type == NUMBER || type == BOOLEAN)
            switch (cond) {
                case CONTAINS:
                case NOT_CONTAIN:
                case START_WITH:
                case ENDS_WITH:
                case SIMILAR_20:
                case SIMILAR_50:
                case SIMILAR_80:
                    return tblName + "::text";
            }
        else if (valuesSize > 1)
            return tblName + "::text";

        return tblName;
    }

    //**************************************************************************
    @SuppressWarnings("unchecked")
    static <T extends Enum<T>> boolean oneOfEnum(T value, T... enumValues) {
        for (T enumVal : enumValues)
            if (value == enumVal)
                return true;

        return false;
    }
}

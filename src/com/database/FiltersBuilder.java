package com.database;

import com.utils.text.StrWriter;
import java.util.*;

/**
 * Klasa tworzy listę filtrów dla sekcji WHERE zapytania
 *
 */
public class FiltersBuilder {

    boolean ignoreSpaces;
    public final List<String> filters = new LinkedList<>();

    public FiltersBuilder(boolean ignoreSpaces) {
        this.ignoreSpaces = ignoreSpaces;
    }

    /**
     * argument powinien być zapisywany w postyaci "warunek = '%s'" Jako
     * parametr dynamiczny zawsze nalezy podawać %s
     */
    public FiltersBuilder add(String argument, Object value, Boolean isText) {
        if (value == null || argument == null || argument.trim().isEmpty())
            return this;

        if (ignoreSpaces && value.toString().trim().isEmpty())
            return this;

        boolean esc = (isText != null && isText) || (isText == null && (value instanceof String));

        String val = value.toString();
        if (esc)
            val = Database.escapeSQL(val);

        filters.add(String.format(argument, val));

        return this;
    }

    public FiltersBuilder add(String argument, Object value) {
        return add(argument, value, null);
    }

    @Override
    public String toString() {
        return getQuery("AND", "  ");
    }

    public String getQuery(String logic, String intent) {
        StrWriter res = new StrWriter();
        boolean first = true;
        for (String s : filters) {
            if (!first)
                res.append("\n");
            res.append(intent);
            if (!first)
                res.append(logic).append(" ");
            res.append("(").append(s).append(")");

            first = false;
        }
        return res.toString();
    }
}

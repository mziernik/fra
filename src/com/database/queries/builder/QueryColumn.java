package com.database.queries.builder;

import com.lang.LDatabase;
import com.utils.Utils;
import com.utils.Is;
import java.util.Objects;

public class QueryColumn {

    public final String name;
    public final String cast;

    public QueryColumn(String name) {
        if (name == null)
            throw new NullPointerException(LDatabase.LACK_OF_COLUMN_NAME.toString());

        String cast = null;

        name = name.trim();

        if (!name.startsWith("\"") && !name.endsWith("\"")) {
            String[] split = name.split("::");
            if (split.length == 2) {
                name = split[0].trim();
                cast = split[1].trim();
                if (cast.isEmpty())
                    cast = null;
            }
        }

        this.name = Objects.requireNonNull(name, LDatabase.LACK_OF_COLUMN_NAME.toString());
        this.cast = cast;
    }

    public boolean isArray() {
        return Utils.coalesce(cast).contains("[]");
    }

    @Override
    public String toString() {
        return name + (!Is.empty(cast) ? "::" + cast : "");
    }

}

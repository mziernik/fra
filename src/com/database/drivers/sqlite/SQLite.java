package com.database.drivers.sqlite;

import com.utils.Utils;
import com.utils.Is;
import com.database.DBConnectionData;
import com.database.Database;
import com.database.elements.JdbcDriver;
import com.database.queries.InsertOrReplace;
import com.database.queries.builder.QueryObject;
import com.utils.Undefined;
import com.utils.text.StrWriter;
import java.text.SimpleDateFormat;
import java.util.*;

import static com.database.Database.escapeSQL;

/**
 * @author Miłosz Ziernik
 * @date 06 listopada 2015
 * @encoding UTF-8
 */
@JdbcDriver("org.sqlite.JDBC")
public class SQLite extends Database {

    public SQLite(DBConnectionData connData) {
        super(connData, SQLiteMeta.class);
    }

    public InsertOrReplace insertOrReplace(String table) {
        return new InsertOrReplace(this, table);
    }

    @Override
    public StrWriter escape(StrWriter qry, QueryObject obj) {

        Object value = obj.getValue();

        if (value == null)
            return qry.append("null");

        if (value == Undefined.TYPE)
            return qry; // pomiń wartość

        if (!obj.needEscape())
            return qry.append(value.toString());

        try {

            List<Object> list = obj.isCollection() ? obj.asCollection() : null;

            if (list != null) {
                if (Boolean.TRUE.equals(obj.array()))
                    qry.append("ARRAY[");

                boolean first = true;
                for (Object o : list) {
                    if (!first)
                        qry.append(", ");

                    qry.nextLevel(() -> {
                        escape(qry, new QueryObject(this, o).array(true));
                    });

                    first = false;
                }

                if (Boolean.TRUE.equals(obj.array()))
                    qry.append("]");

                return qry;
            }

            if (value instanceof java.util.Date)
                value = new SimpleDateFormat(dateFormat)
                        .format((java.util.Date) value);

            qry.append("'");
            qry.append(escapeSQL(Utils.toString(value)));
            qry.append("'");

        } finally {
            if (!Is.empty(obj.cast()))
                qry.append("::").append(obj.cast());
        }
        return qry;
    }
}

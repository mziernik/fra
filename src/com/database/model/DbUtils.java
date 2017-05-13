package com.database.model;

import com.database.Database;
import com.database.QueryRow;
import com.database.QueryRows;
import com.database.queries.Query;
import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.intf.runnable.RunnableEx2;
import java.sql.SQLException;
import java.util.Objects;

public class DbUtils {

    public static Query addMarker(Database db, Object obj) {
        return new Query(db, "SELECT ? AS class_name, ? AS hash_code",
                obj.getClass().getName(), obj.hashCode());
    }

    public static <T> void processMarkers(QueryRows rows, Iterable<T> objects,
            RunnableEx2<QueryRows, T> callback) throws SQLException {

        while (rows != null) {
            if (rows.size() != 1)
                throw new SQLException("Unexpected results count (" + rows.size() + ")");
            QueryRow row = rows.firstD();

            if (rows.size() != 1 || rows.columns.length != 2
                    || !"class_name".equals(rows.columns[0].name)
                    || !"hash_code".equals(rows.columns[1].name))
                throw new ServiceException("Unexpected data");

            rows = rows.nextResults();

            String cn = row.getStr("class_name");
            int hc = row.getInt("hash_code");

            for (T rec : objects)
                try {
                    if (!rec.getClass().getName().equals(cn) || rec.hashCode() != hc)
                        continue;

                    QueryRows qr = Objects.requireNonNull(rows, "No results");

                    callback.run(qr, rec);
                    rows = rows.nextResults();

                    break;

                } catch (Throwable e) {
                    throw new ThrowableException(e)
                            .details("Object", rec.getClass().getName());
                }
        }
    }

}

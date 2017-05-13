package com.database.cache;

import com.dev.Dev;
import com.database.QueryRow;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.orm.DbTable;
import com.exceptions.ThrowableException;
import com.servlet.Handlers;
import com.utils.collections.Strings;
import com.utils.reflections.TClass;
import java.sql.SQLException;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 14 września 2015
 * @encoding UTF-8
 */
public class CachedTableManager {

    public static int timeTolerance = 30 * 1000; // co jaki czas słowniki będą odświeżane (30 sek)

    final static Map<Class<? extends CachedTable>, CachedTable<?>> map = new LinkedHashMap<>();

    CachedTable<?> table;

    public CachedTableManager(CachedTable<?> table) {
        this.table = table;
    }

    static long lastUpdateTs = 0;

    static void update(boolean forced) throws SQLException {

        if (!forced && System.currentTimeMillis() - lastUpdateTs < timeTolerance)
            return;

        Dev.info("SQL", "Przeładowuję statystyki słowników");

        PostgreSQL db = Handlers.database.getInstance().getPgConnection(CachedTableManager.class);

        Map<String, String> mm = new HashMap<>();

        for (QueryRow row : db.execute(
                "SELECT schemaname || '.' || relname as table,\n"
                + "        n_tup_upd || ',' || n_tup_ins || ',' || n_tup_del as key\n"
                + "FROM pg_stat_user_tables\n"
                + "ORDER BY schemaname, relname"
        ))
            mm.put(row.getStr("table"), row.getStr("key"));

        for (CachedTable<?> tbl : map.values()) {

            Strings key = new Strings();
            for (DbTable t : tbl.tables)
                key.add(mm.get(t.schemaName + "." + t.name));

            tbl.newKey = key.toString(";");
            if (tbl.currentKey == null)
                tbl.currentKey = tbl.newKey;
        }
        lastUpdateTs = System.currentTimeMillis();
    }

    public static <T extends CachedTable<?>> T get(Class<T> tableClass) throws SQLException {

        PostgreSQL db = Handlers.database.getInstance().getPgConnection(CachedTableManager.class, tableClass);

        CachedTable<?> table;

        table = map.get(tableClass);

        if (table != null)
            update(false);

        if (table == null || !Objects.equals(table.currentKey, table.newKey))
            try {
                table = new TClass<>(tableClass).newInstance(null);
                table.process(table.getRows(db));
                map.put(table.getClass(), table);
                update(true);
            } catch (Exception ex) {
                throw new ThrowableException(ex);
            }

        return (T) table;
    }

}

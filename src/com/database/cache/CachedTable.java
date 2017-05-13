package com.database.cache;

import com.database.*;
import com.database.cache.CachedTable.CachedTableRow;
import com.database.orm.DbTable;
import com.exceptions.ThrowableException;
import com.lang.LDatabase;
import com.utils.reflections.TClass;
import java.sql.SQLException;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @param <Self>
 * @param <Row>
 * @param <Key>
 * @date 14 września 2015
 * @encoding UTF-8
 */
public abstract class CachedTable<Row extends CachedTableRow>
        implements Iterable<Row> {

    private final List<Row> rows = new LinkedList<>();
    protected final Class<? extends Row> rowClass;
    public final DbTable[] tables;

    //----- cache ----------
    String currentKey;
    String newKey;
    long updateTS;
    //---------------

    public CachedTable(Class<? extends Row> rowClass, DbTable... tables) {
        this.rowClass = rowClass;
        this.tables = tables;
        if (tables == null || tables.length == 0)
            throw new UnsupportedOperationException(LDatabase.NO_DEFINED_TABLES.toString());
    }

    public static <T extends CachedTable<? extends CachedTableRow>> T
            instance(Class<? extends T> cls) throws SQLException {
        return CachedTableManager.get(cls);
    }

    protected abstract Database getDatatabse();

    protected abstract QueryRows getRows(Database db) throws SQLException;

    public void process(QueryRows rows) throws SQLException {
        this.rows.clear();
        try {
            for (QueryRow row : rows)
                this.rows.add(new TClass<Row>(rowClass).newInstance(this, row));
        } catch (Exception e) {
            throw new ThrowableException(e);
        }
    }

    @Override
    public Iterator<Row> iterator() {
        return rows.iterator();
    }

    public static class CachedTableRow<T extends CachedTable<?>> {

        public CachedTableRow(T table, QueryRow row) throws SQLException {

        }

        protected CachedTableRow() {

        }

    }

}

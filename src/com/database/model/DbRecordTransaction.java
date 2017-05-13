package com.database.model;

import com.database.Database;
import com.database.QueryRow;
import com.database.QueryRows;
import com.database.queries.MultipleQuery;
import com.exceptions.SQLError;
import com.model.dataset.DsColumn;
import com.model.dataset.DsRecord;
import com.utils.collections.TList;
import java.sql.SQLException;
import java.util.*;
import java.util.Map.Entry;

public class DbRecordTransaction {

    final Map<DsTable<?, ?>, Map<DsColumn<?, ? extends DsTable<?, ?>, QueryRow, ?>, Object>> update = new LinkedHashMap<>();
    final Set<DsRecord<?, QueryRow, ?>> delete = new LinkedHashSet<>();

    public <T> DbRecordTransaction set(DsColumn<?, ? extends DsTable<?, ?>, QueryRow, T> cell, T value) {
        Map<DsColumn<?, ? extends DsTable<?, ?>, QueryRow, ?>, Object> map = update.get(cell.getParent());
        if (map == null) {
            map = new LinkedHashMap<>();
            update.put(cell.getParent(), map);
        }

        map.put(cell, value);
        return this;
    }

    public DbRecordTransaction delete(DsTable<?, ?> tbl) {
        delete.add(tbl.getRecord());
        return this;
    }

    public void commit(Database db) throws SQLError, SQLException {

        db.transaction((d) -> {
            final Set<DsTable> sortTables = new HashSet<>();

            MultipleQuery mqry = d.multipleQuery();

            for (Entry<DsTable<?, ?>, Map<DsColumn<?, ? extends DsTable<?, ?>, QueryRow, ?>, Object>> en : update.entrySet()) {
                DsTable<? extends DsTable<?, ?>, ?> tbl = en.getKey();
                mqry.add(DbUtils.addMarker(d, tbl));
                tbl.getUpdateQuery(mqry, (Map) en.getValue());
            }

            for (DsRecord<?, QueryRow, ?> rec : delete) {
                mqry.add(DbUtils.addMarker(d, ((DsTable) rec.dataSet)));
                ((DsTable) rec.dataSet).getDeleteQuery(mqry, rec);
            }
            QueryRows rows = mqry.execute();

            DbUtils.processMarkers(rows, update.keySet(), (qr, tbl) -> {
                if (qr.size() != 1)
                    throw new SQLException("Unexpected results count (" + qr.size() + ")");
                tbl.fillRow(Objects.requireNonNull(qr.first()));
            });

            for (DsRecord<?, QueryRow, ?> rec : delete)
                ((DsTable) rec.dataSet)._removeRecord(rec);

            // przepisanie danych z tabeli tymczasowej do mastera
            for (Entry<DsTable<?, ?>, Map<DsColumn<?, ? extends DsTable<?, ?>, QueryRow, ?>, Object>> en : update.entrySet()) {
                DsTable tbl = DsTable.getTableF((Class) en.getKey().getClass());
                tbl.apply(en.getKey(), en.getValue());
                sortTables.add(tbl);
            }

            for (DsTable tbl : sortTables)
                tbl.sort();
        });

    }
}

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

    final Map<Repository_old<?, ?>, Map<DsColumn<?, ? extends Repository_old<?, ?>, QueryRow, ?>, Object>> update = new LinkedHashMap<>();
    final Set<DsRecord<?, QueryRow, ?>> delete = new LinkedHashSet<>();

    public <T> DbRecordTransaction initializeInsert(Repository_old<?, ?> table) {
        Map<DsColumn<?, ? extends Repository_old<?, ?>, QueryRow, ?>, Object> map = update.get(table);
        if (map == null) {
            map = new LinkedHashMap<>();
            update.put(table, map);
        }
        return this;
    }

    public <T> DbRecordTransaction set(DsColumn<?, ? extends Repository_old<?, ?>, QueryRow, T> cell, T value) {
        Map<DsColumn<?, ? extends Repository_old<?, ?>, QueryRow, ?>, Object> map = update.get(cell.getParent());
        if (map == null) {
            map = new LinkedHashMap<>();
            update.put(cell.getParent(), map);
        }

        map.put(cell, value);
        return this;
    }

    public DbRecordTransaction delete(Repository_old<?, ?> tbl) {
        delete.add(tbl.getRecord());
        return this;
    }

    public void commit(Database db) throws SQLError, SQLException {

        db.transaction((d) -> {
            final Set<Repository_old> sortTables = new HashSet<>();

            MultipleQuery mqry = d.multipleQuery();

            for (Entry<Repository_old<?, ?>, Map<DsColumn<?, ? extends Repository_old<?, ?>, QueryRow, ?>, Object>> en : update.entrySet()) {
                Repository_old<? extends Repository_old<?, ?>, ?> tbl = en.getKey();
                mqry.add(DbUtils.addMarker(d, tbl));
                tbl.getUpdateQuery(mqry, (Map) en.getValue());
            }

            for (DsRecord<?, QueryRow, ?> rec : delete) {
                mqry.add(DbUtils.addMarker(d, ((Repository_old) rec.dataSet)));
                ((Repository_old) rec.dataSet).getDeleteQuery(mqry, rec);
            }

            if (mqry.isEmpty())
                return;

            QueryRows rows = mqry.execute();

            DbUtils.processMarkers(rows, update.keySet(), (qr, tbl) -> {
                if (qr.size() != 1)
                    throw new SQLException("Unexpected results count (" + qr.size() + ")");
                tbl.fillRow(Objects.requireNonNull(qr.first()));
            });

            for (DsRecord<?, QueryRow, ?> rec : delete)
                ((Repository_old) rec.dataSet)._removeRecord(rec);

            // przepisanie danych z tabeli tymczasowej do mastera
            for (Entry<Repository_old<?, ?>, Map<DsColumn<?, ? extends Repository_old<?, ?>, QueryRow, ?>, Object>> en : update.entrySet()) {
                Repository_old tbl = Repository_old.getRepoF((Class) en.getKey().getClass());
                tbl.apply(en.getKey(), en.getValue());
                sortTables.add(tbl);
            }

            for (Repository_old tbl : sortTables)
                tbl.sort();
        });

    }
}

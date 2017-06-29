package com.model.repository;

import com.model.repository.intf.CRUDE;
import com.utils.date.TDate;
import java.util.Objects;

public class RecordUpdate extends Record {

    public RecordUpdate(Repository<?> repo, CRUDE crude, Object[] cells) {
        super(repo, crude, cells);
    }

    public RecordUpdate update() {
        Object pk = Objects.requireNonNull(getPrimaryKeyValue());

        crude = ((Repository) repo).has(pk) ? CRUDE.UPDATE : CRUDE.CREATE;

        repo.updateRecord(this);

        repo.lastUpdate = new TDate();
        repo.lastUpdatedBy = "root";
        synchronized (repo.updatesCount) {
            repo.updatesCount.incrementAndGet();
        }
        return this;
    }

    public RecordUpdate delete() {

        return this;
    }

    @Override
    public <T> RecordUpdate set(Column<T> field, T value) {
        super.set(field, value);
        return this;
    }

}

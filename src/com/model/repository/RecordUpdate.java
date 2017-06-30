package com.model.repository;

import com.model.repository.intf.CRUDE;
import com.thread.QueueThread;
import com.utils.collections.MapList;
import com.utils.collections.TList;
import com.utils.date.TDate;
import java.util.Objects;

public class RecordUpdate extends Record {

    final static Queue QUEUE = new Queue();

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
        QUEUE.add(this);
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

class Queue extends QueueThread<Record> {

    public Queue() {
        super("RecordUpdate");
        minDelay = 1000;
    }

    @Override
    protected void processItem(Record item) throws Exception {
        TList<Record> queue = clearQueue();
        if (queue.isEmpty())
            return;

        MapList<Repository<?>, Record> repos = new MapList<>();
        queue.forEach(rec -> repos.add(rec.repo, rec));
        RepoTransaction.webApiBroadcast(repos);
    }

}

package com.model.repository;

import com.model.RRepoSate;
import com.model.repository.intf.CRUDE;
import com.thread.QueueThread;
import com.utils.collections.MapList;
import com.utils.collections.TList;
import java.util.Objects;

public class RecordUpdate extends Record {

    private final static Queue BROADCAST_QUEUE = new Queue();

    public RecordUpdate(Repository<?> repo, CRUDE crude, Object[] cells) {
        super(repo, crude, cells);
    }

    public RecordUpdate update() {
        Object pk = Objects.requireNonNull(getPrimaryKeyValue());
        crude = ((Repository) repo).has(pk) ? CRUDE.UPDATE : CRUDE.CREATE;
        repo.updateRecord(this, true);
        BROADCAST_QUEUE.add(this);
        return this;
    }

    public RecordUpdate delete() {
        crude = CRUDE.DELETE;
        repo.updateRecord(this, true);
        BROADCAST_QUEUE.add(this);
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
        minDelay = 500;
    }

    @Override
    public void add(Record rec) {
        if (!RRepoSate.canUpdate(rec.repo))
            return;
        super.add(rec);
    }

    @Override
    protected void processItem(Record item) throws Exception {
        TList<Record> queue = fetchQueue(true);

        MapList<Repository<?>, Record> repos = new MapList<>();
        queue.forEach(rec -> repos.add(rec.repo, rec));
        ReposTransaction.webApiBroadcast(repos);
    }

}

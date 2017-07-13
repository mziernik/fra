package com.model.repository;

import com.model.dao.core.DAO;
import com.model.repository.intf.CRUDE;

public class RepoTransaction<PK> extends AbstractRepoTransaction {

    private final Repository<PK> repo;

    public RepoTransaction(Repository<PK> repo) {
        this.repo = repo;
    }

    public Record action(CRUDE crude, DAO dao) throws Exception {
        return super.action(repo, crude, dao);
    }

    public Record create() {
        return super.create(repo);
    }

    public Record createOrUpdate(PK pk) {
        return super.createOrUpdate(repo, pk);
    }

    public Record delete(PK pk) {
        return super.delete(repo, pk);
    }

    public Record getRecord(PK pk, boolean mustExists) {
        return super.getRecord(repo, pk, mustExists);
    }

    public Record update(PK pk) {
        return super.update(repo, pk);
    }

}

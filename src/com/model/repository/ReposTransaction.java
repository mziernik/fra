package com.model.repository;

import com.model.dao.core.DAO;
import com.model.repository.intf.CRUDE;

public class ReposTransaction extends AbstractRepoTransaction {

    @Override
    public <PK> Record action(Repository<PK> repo, CRUDE crude, DAO dao) throws Exception {
        return super.action(repo, crude, dao);
    }

    @Override
    public <PK> Record create(Repository<PK> repo) {
        return super.create(repo);
    }

    @Override
    public <PK> Record createOrUpdate(Repository<PK> repo, PK pk) {
        return super.createOrUpdate(repo, pk);
    }

    @Override
    public <PK> Record delete(Repository<PK> repo, PK pk) {
        return super.delete(repo, pk);
    }

    @Override
    public <PK> Record getRecord(Repository<PK> repo, PK pk, boolean mustExists) {
        return super.getRecord(repo, pk, mustExists);
    }

    @Override
    public <PK> Record update(Repository<PK> repo, PK pk) {
        return super.update(repo, pk);
    }

}

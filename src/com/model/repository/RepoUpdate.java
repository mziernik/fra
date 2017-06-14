package com.model.repository;

import com.utils.collections.TList;

public class RepoUpdate<REPO extends Repository<PRIMARY_KEY>, PRIMARY_KEY> {
    
    public final REPO repository;
    public final TList<Record> records = new TList<>();
    
    public RepoUpdate(REPO repository) {
        this.repository = repository;
    }
    
    public Record createOrUpdate(PRIMARY_KEY pk) {
        return records.addR(repository.createOrUpdate(pk));
    }
    
    public Record create() {
        return records.addR(repository.create());
    }
    
    public Record update(PRIMARY_KEY pk) {
        return records.addR(repository.update(pk));
    }
    
    public Record delete(PRIMARY_KEY pk) {
        return records.addR(repository.delete(pk));
    }
    
    public void commit(boolean strict) throws Exception {
        Repository.commit(records);
    }
}

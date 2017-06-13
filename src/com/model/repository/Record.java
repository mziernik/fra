package com.model.repository;

import com.database.QueryRow;

public class Record {

    public final Repository<?> repo;
    public final CRUDE crude;
    Object[] cells;

    public Record(Repository<?> repo, CRUDE crude, Object[] cells) {
        this.repo = repo;
        this.crude = crude;
        this.cells = cells;
    }

    public String getId() {
        return null;
    }

    public Object getPrimaryKeyValue() {
        int idx = repo.fields.indexOf(repo.primaryKey);
        if (idx < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada klucza głównego");
        return cells[idx];
    }

    public <T> T get(RepoField<T> field) {
        int idx = repo.fields.indexOf(field);
        if (idx < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada kolumny " + field.key);

        return (T) cells[idx];
    }

    public Record setAny(RepoField<?> field, Object value) {
        return _set(field, field.adapter.process(value));

    }

    public <T> Record set(RepoField<T> field, T value) {
        return _set(field, value);
    }

    private Record _set(RepoField<?> field, Object value) {
        if (crude != CRUDE.CREATE && crude != CRUDE.UPDATE)
            throw new RepositoryException(repo, "Nie można modyfikować rekordu " + getId());

        if (crude == CRUDE.UPDATE && field == repo.primaryKey)
            throw new RepositoryException(repo, "Nie można modyfikować klucza głównego");

        int idx = repo.fields.indexOf(field);
        if (idx < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada kolumny " + field.key);

        cells[idx] = value;
        return this;
    }

    void fillRow(QueryRow requireNonNull) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

}

package com.model.repository;

import com.model.repository.intf.CRUDE;
import com.utils.collections.TList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class Record implements Iterable<Column<?>> {

    public final Repository<?> repo;
    public final CRUDE crude;
    Object[] cells;
    final Set<Column<?>> changed = new HashSet<>();

    public Record(Repository<?> repo, CRUDE crude, Object[] cells) {
        this.repo = repo;
        this.crude = crude;
        this.cells = cells;
    }

    public String getId() {
        return null;
    }

    public int indexOf(Column<?> column) {
        return new TList(repo.columns.values()).indexOf(column);
    }

    public Object getPrimaryKeyValue() {
        int idx = indexOf(repo.config.primaryKey);
        if (idx < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada klucza głównego");
        return cells[idx];
    }

    public boolean isChanged(Column<?> field) {
        return changed.contains(field);
    }

    public <T> T get(Column<T> field) {
        int idx = indexOf(field);
        if (idx < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada kolumny " + field.getKey());

        return (T) cells[idx];
    }

    public <T> Record setAny(Column<T> field, Object value) {
        if (crude != CRUDE.CREATE && crude != CRUDE.UPDATE)
            throw new RepositoryException(repo, "Nie można modyfikować rekordu " + getId());

        if (crude == CRUDE.UPDATE && field == repo.config.primaryKey) {
            Object pk = getPrimaryKeyValue();
            if (!pk.equals(value))
                throw new RepositoryException(repo, "Nie można modyfikować klucza głównego");
            return this;
        }
        field.set(this, value);
        return this;
    }

    public <T> Record set(Column<T> field, T value) {
        return setAny(field, value);
    }

    @Override
    public Iterator<Column<?>> iterator() {
        return repo.columns.values().iterator();
    }

}

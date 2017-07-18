package com.model.repository;

import com.model.repository.intf.CRUDE;
import com.utils.Utils;
import com.utils.collections.Pair;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class Record implements Iterable<Column<?>> {

    public final Repository<?> repo;
    public CRUDE crude;
    Object[] cells;
    public final LinkedHashMap<Column<?>, Pair<Object, Object>> changed = new LinkedHashMap<>();
    private final int pkIndex;

    public Record(Repository<?> repo, CRUDE crude, Object[] cells) {
        if (cells == null)
            cells = new Object[repo.columns.size()];
        this.repo = repo;
        this.crude = crude;
        this.cells = cells;
        this.pkIndex = indexOf(repo.config.primaryKey);
        if (pkIndex < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada klucza głównego");
    }

    /**
     * Zwraca ID obiektu w postaci repozytorium[pk=wartość]
     */
    public String toString() {
        return repo.config.key + "[" + repo.config.primaryKey.config.key + "="
                + Utils.escape(getPrimaryKeyValue()) + "]";
    }

    public int indexOf(Column<?> column) {
        return new TList(repo.columns.values()).indexOf(column);
    }

    public Object getPrimaryKeyValue() {
        return cells[pkIndex];
    }

    public boolean isChanged(Column<?> column) {
        return changed.containsKey(column);
    }

    public <T> T get(Column<T> column) {
        int idx = indexOf(column);
        if (idx < 0)
            throw new RepositoryException(repo, "Repozytorium nie posiada kolumny " + column.getKey());

        return (T) cells[idx];
    }

    public <T> Object serialize(Column<T> column) {
        T value = get(column);
        DataType<T> type = (DataType<T>) column.config.type;
        return type.serialize(value);
    }

    public <T> Record setAny(Column<T> field, Object value) {
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

    public void validate() {
        AtomicInteger idx = new AtomicInteger(0);
        repo.columns.values().forEach(col -> col.validate(this, cells[idx.getAndIncrement()]));
    }

}

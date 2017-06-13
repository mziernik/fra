package com.model.repository;

import com.intf.callable.Callable1;
import com.json.Escape;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.utils.collections.Pair;
import com.utils.collections.Pairs;
import com.utils.collections.TList;
import com.utils.text.StrWriter;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;

public class Repository<PRIMARY_KEY> {

    public final String key;
    public final CharSequence name;
    private final String tableName;
    public RepoField<PRIMARY_KEY> primaryKey;
    private final AtomicInteger hash = new AtomicInteger(0); // identator zmiany
    protected boolean autoUpdate; //czy informaje o zmianach mają być automatycznie rozsyłane do klientów

    protected final Pairs<RepoField<?>, Boolean> orderColumns = new Pairs<>(); // kolumna, ASC

    private final HashMap<PRIMARY_KEY, Object[]> records = new HashMap<>();
    final TList<RepoField<?>> fields = new TList<>();

    public Repository(String key, CharSequence name, RepoField<PRIMARY_KEY> primaryKey) {
        this(key, null, name, primaryKey);
    }

    //ToDo: statyczna definicja pól
    protected Repository(String key, String tableName, CharSequence name, RepoField<PRIMARY_KEY> primaryKey) {
        this.key = key;
        this.name = name;
        this.primaryKey = primaryKey;
        this.tableName = tableName;

        for (Field f : getClass().getDeclaredFields())
            if (RepoField.class.isAssignableFrom(f.getType()))
                try {
                    f.setAccessible(true);
                    fields.add((RepoField<?>) f.get(this));
                } catch (Throwable e) {
                    throw new RepositoryException(this, e);
                }
    }

    protected DAOQuery getSelectQuery(DAO dao) {

        DAOQuery qry = new DAOQuery(dao, CRUDE.READ);
        qry.source(tableName);

        for (RepoField<?> col : fields)
            qry.field(col.getStoreName());

        for (Pair<RepoField<?>, Boolean> pair : orderColumns)
            qry.order(pair.first.getStoreName(), pair.second);

        return qry;
    }

    public TList<Record> select(Predicate<Record> predicate) {
        TList<Record> result = new TList<>();
        forEach(rec -> {
            if (predicate.test(rec))
                result.add(new Record(this, CRUDE.READ, rec.cells));
            return true;
        });
        return result;
    }

    public Record create() {
        return new Record(this, CRUDE.CREATE, new Object[fields.size()]);
    }

    private Object[] getCells(PRIMARY_KEY pk, boolean clone) {
        Object[] cells;
        synchronized (records) {
            cells = records.get(pk);
        }
        if (cells == null)
            throw new RepositoryException(this, "Nie znaleziono rekordu " + Escape.escape(pk));
        return clone ? cells.clone() : cells;
    }

    public Record read(PRIMARY_KEY pk) {
        return new Record(this, CRUDE.UPDATE, getCells(pk, false));
    }

    public Record update(PRIMARY_KEY pk) {
        return new Record(this, CRUDE.UPDATE, getCells(pk, true));
    }

    public Record delete(PRIMARY_KEY pk) {
        return new Record(this, CRUDE.DELETE, getCells(pk, false));
    }

    public void forEach(Callable1<Boolean, Record> consumer) {
        final Record rec = new Record(this, CRUDE.READ, null);
        TList<Object[]> rows;
        synchronized (records) {
            rows = new TList<>(this.records.values());
        }

        for (Object[] cells : rows) {
            rec.cells = cells;
            if (Boolean.FALSE.equals(consumer.run(rec)))
                return;
        }
    }

    public static void commit(Record... records) throws Exception {
        commit(Arrays.asList(records));
    }

    public static void commit(Collection<Record> records) throws Exception {

        TList<Record> db = new TList<>();
        TList<Record> local = new TList<>();
        for (Record rec : records)
            if (rec.repo instanceof DbRepo)
                db.add(rec);
            else
                local.add(rec);

        if (!db.isEmpty())
            DbRepo.updateRecords(db);

        for (Record rec : local)
            rec.repo.updateRecord(rec);
    }

    /**
     * Dodaje lub modyfikuje rekord
     */
    protected void updateRecord(Record record) {
        if (record.repo != this)
            throw new RepositoryException(this, "Rekord nie należy do repozytorium");

        PRIMARY_KEY pk = (PRIMARY_KEY) record.getPrimaryKeyValue();
        synchronized (records) {
            switch (record.crude) {
                case CREATE:
                    if (records.containsKey(pk))
                        throw new RepositoryException(this, "Rekord " + record.getId() + " już istnieje");
                    records.put(pk, record.cells.clone());
                    return;

                case UPDATE:
                    Object[] cells = getCells(pk, false);
                    for (int i = 0; i < cells.length; i++)
                        cells[i] = record.cells[i];
                    return;

                case DELETE:
                    Object[] cc = records.remove(pk);
                    if (cc == null)
                        throw new RepositoryException(this, "Nie znaleziono rekordu " + Escape.escape(pk));
                    return;

                default:
                    throw new RepositoryException(this, new UnsupportedOperationException(record.crude.name()));
            }
        }
    }

    public void sort() {

    }

    public void load(DAORows<?> rows) {

        TList<Record> records = new TList<>();

        for (DAORow row : rows) {

            Record rec = new Record(this, CRUDE.CREATE, new Object[fields.size()]);

            for (RepoField<?> field : fields) {
                Object value = row.getValue(field.getStoreName(), null);
                rec.setAny(field, value);
            }
            records.add(rec);
        }

        for (Record rec : records) {
            PRIMARY_KEY pk = (PRIMARY_KEY) rec.getPrimaryKeyValue();
            this.records.put(pk, rec.cells.clone());
        }

    }

}

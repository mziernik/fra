package com.model.repository;

import com.events.Dispatcher;
import com.model.repository.intf.CRUDE;
import com.intf.callable.Callable1;
import com.intf.runnable.Runnable1;
import com.intf.runnable.RunnableEx2;
import com.json.JArray;
import com.json.JObject;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.utils.Utils;
import com.utils.collections.*;
import com.utils.date.TDate;
import com.utils.reflections.TField;
import com.webapi.core.WebApiController;
import com.webapi.core.client.Repositories;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import javassist.Modifier;

public class Repository<PRIMARY_KEY> {

    public final static SyncMap<String, Repository<?>> ALL = new SyncMap<>();
    public final RepoConfig config = new RepoConfig();
    private final AtomicInteger hash = new AtomicInteger(0); // identator zmiany
    protected final Pairs<Column<?>, Boolean> orderColumns = new Pairs<>(); // kolumna, ASC
    public final Dispatcher<RunnableEx2<List<Record>, MapList<Repository<?>, Record>>> onBeforeUpdate = new Dispatcher<>();

    final HashMap<PRIMARY_KEY, Object[]> records = new HashMap<>();
    final Map<String, Column<?>> columns = new LinkedHashMap<>();
    // ------------ statystyki ------------
    TDate lastUpdate;
    final AtomicInteger updatesCount = new AtomicInteger();
    String lastUpdatedBy;

    public class RepoConfig {

        public String key;
        public CharSequence name;
        public String daoName;
        public Column<?> primaryKey;
        public Column<?> displayName;
        public boolean autoUpdate = true; //czy informaje o zmianach mają być automatycznie rozsyłane do klientów
        public DAO dao;
        public Boolean local;
        public Integer limit;
        public Integer offset;
        public Boolean dynamic;
        public final Flags<CRUDE> crude = CRUDE.flags(CRUDE.CRUD);

        public RepoConfig() {
            if (Repository.this instanceof DynamicRepo)
                dynamic = true;
        }

        public void validate() {
            Objects.requireNonNull(key, Repository.this.getClass() + ": key");
            Objects.requireNonNull(name, Repository.this.getClass() + ": name");
            Objects.requireNonNull(primaryKey, Repository.this.getClass() + ": primaryKey");
        }

        public void order(Column<?> column, boolean ascendant) {

        }

        public void view(Column<?>... columns) {

        }

        public Params getClinetParams() {
            return new Params()
                    .escape("key", key)
                    .escape("name", name)
                    .add("record", Repository.this.getClass().getSimpleName() + "Record")
                    .add("primaryKeyColumn", Repository.this.getClass().getSimpleName()
                            + "." + Repositories.formatFieldName(primaryKey.getKey()))
                    .add("displayNameColumn", displayName != null
                            ? Repository.this.getClass().getSimpleName()
                            + "." + Repositories.formatFieldName(displayName.getKey())
                            : null)
                    .escape("crude", crude.getChars())
                    .add("local", local)
                    .add("autoUpdate", autoUpdate);
        }

        public void getJson(JObject obj, boolean metaData) {

            if (metaData) {
                obj.put("key", key);
                obj.put("name", name);
                obj.put("local", local);
                obj.put("autoUpdate", autoUpdate);
            }
            obj.put("rowsCount", records.size());
            obj.put("lastUpdated", lastUpdate != null ? lastUpdate.getTime() : null);
            obj.put("lastUpdatedBy", lastUpdatedBy);
            obj.put("updates", updatesCount.get());
            obj.put("crude", new Strings().map(crude, cr -> "" + cr.name().charAt(0)).toString(""));

            JArray jcol = obj.arrayC("columns");
            for (Column<?> column : columns.values())
                jcol.add(metaData ? column.getJson() : column.getKey());
        }

    }

    //ToDo: statyczna definicja pól
    public Repository(Runnable1<RepoConfig> cfg) {
        cfg.run(config);
        config.validate();

        config.primaryKey.config.unique = true;
        config.primaryKey.config.required = true;
        Objects.requireNonNull(config.primaryKey.config.daoName);

        for (Field f : getClass().getDeclaredFields())
            if (Column.class.isAssignableFrom(f.getType()))
                try {
                    f.setAccessible(true);
                    TField tf = new TField(f);
                    tf.checkModifiers(Modifier.STATIC);
                    Column<?> col = (Column<?>) f.get(this);

                    if (col.repository != null)
                        throw new RepositoryException(this, "Kolumna " + col.getKey()
                                + " ma już przypisane repozytorium " + col.repository.getKey());

                    if (columns.containsKey(col.getKey()))
                        throw new RepositoryException(this, "Repozytorium zawiera już kolumnę " + col.getKey());
                    columns.put(col.getKey(), col);

                    col.repository = this;
                } catch (Throwable e) {
                    throw new RepositoryException(this, e);
                }
    }

    public boolean isEmpty() {
        return records.isEmpty();
    }

    public int size() {
        return records.size();
    }

    public boolean has(PRIMARY_KEY pk) {
        return records.containsKey(pk);
    }

    public int getUpdatesCount() {
        return updatesCount.get();
    }

    protected boolean beforeCommit(TList<Record> get) {
        return true;
    }

    protected boolean beforeBroadcast(TList<WebApiController> recipients) {
        return true;
    }

    protected DAOQuery fillQuery(DAOQuery qry, Record rec) {
        if (config.daoName == null)
            return qry;
        qry.source(config.daoName);

        switch (qry.crude) {

            case READ:
                for (Column<?> col : columns.values())
                    if (col.config.daoName != null)
                        qry.field(col.config.daoName);

                for (Pair<Column<?>, Boolean> pair : orderColumns)
                    if (pair.first.config.daoName != null)
                        qry.order(pair.first.config.daoName, pair.second);

                return qry;

            case CREATE:
            case UPDATE:
                qry.primaryKeyName = rec.repo.config.primaryKey.config.daoName;
                qry.primaryKeyValue = rec.getPrimaryKeyValue();

                for (Column<?> col : columns.values()) {
                    if (col.config.daoName == null)
                        continue;

                    qry.field(col.config.daoName);

                    if (!rec.isChanged(col))
                        continue;

                    Object value = rec.serialize(col);
                    qry.param(col.config.daoName, value);
                }

                return qry;

            case DELETE:
                qry.primaryKeyName = rec.repo.config.primaryKey.config.daoName;
                qry.primaryKeyValue = rec.getPrimaryKeyValue();
                return qry;

            default:
                throw new UnsupportedOperationException(qry.crude.name());
        }
    }

    protected void fillRecord(Record rec, DAORow row) {
        for (Column<?> field : columns.values()) {
            if (field.config.daoName == null)
                continue;
            Object value = row.getValue(field.config.daoName, null);
            rec.setAny(field, value);
        }
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

    Object[] getCells(PRIMARY_KEY pk, boolean clone) {
        Object[] cells;
        synchronized (records) {
            cells = records.get(pk);
        }
        if (cells == null)
            throw new RepositoryException(this, "Nie znaleziono rekordu " + Utils.escape(pk));
        return clone ? cells.clone() : cells;
    }

    public Record read(PRIMARY_KEY pk) {
        return new Record(this, CRUDE.READ, getCells(pk, false));
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
                        throw new RepositoryException(this, "Rekord " + record + " już istnieje");
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
                        throw new RepositoryException(this, "Nie znaleziono rekordu " + Utils.escape(pk));
                    return;

                default:
                    throw new RepositoryException(this, new UnsupportedOperationException(record.crude.name()));
            }
        }
    }

    static Repository<?> getF(String name) {
        Repository<?> repo = ALL.get(name);
        if (repo == null)
            throw new RepositoryException(null, "Nie znaleziono repozytorium " + Utils.escape(name));
        return repo;
    }

    public void sort() {

    }

    public void load(DAORows<?> rows) {

        TList<Record> records = new TList<>();

        for (DAORow row : rows) {
            Record rec = new Record(this, CRUDE.CREATE, new Object[columns.size()]);
            fillRecord(rec, row);
            records.add(rec);
        }

        for (Record rec : records) {
            PRIMARY_KEY pk = (PRIMARY_KEY) rec.getPrimaryKeyValue();
            this.records.put(pk, rec.cells.clone());
        }

    }

    public static <REPO extends Repository<?>> REPO register(REPO repo) {
        String key = repo.getKey();
        if (ALL.containsKey(key))
            throw new RepositoryException(null, "Repozytorium "
                    + Utils.escape(key) + " już istnieje");
        ALL.put(key, repo);
        return repo;
    }

    public static void loadAll(DAO<?> dao) throws Exception {

        TList<DAOQuery> queries = new TList<>();
        for (Repository<?> repo : ALL.values()) {
            if (Boolean.TRUE.equals(repo.config.local) || repo.config.daoName == null)
                continue;
            repo.config.local = false;
            repo.config.dao = dao;

            queries.add(repo.fillQuery(new DAOQuery(repo, dao, CRUDE.READ), null));
        }

        TList<? extends DAORows<?>> result = dao.process(queries);

        for (DAORows<?> rows : result) {
            Repository<?> repo = (Repository<?>) rows.context;
            repo.load(rows);
        }

    }

    public String getKey() {
        return config.key;
    }

    public String getName() {
        return config.name.toString();
    }

    public Map<String, Column<?>> getColumns() {
        return columns;
    }

    /**
     * Meta dane repozytorium
     *
     * @return
     */
    public JObject getJson(boolean includeMetaData, boolean includeContent) {
        JObject obj = new JObject(config.key);

        config.getJson(obj, includeMetaData);

        if (!includeContent)
            return obj;

        JArray jrows = obj.arrayC("rows");

        TList<Object[]> data = new TList<>();
        synchronized (records) {
            data = new TList<>(records.values());
        }

        Collection<Column<?>> columns = this.columns.values();

        for (Object[] row : data) {

            JArray jrow = jrows.array();
            jrow.options.singleLine(true);

            int idx = 0;
            for (Column<?> col : columns)
                try {
                    jrow.add(col._serialize(row[idx++]));
                } catch (RuntimeException | Error e) {
                    throw e;
                } catch (Exception e) {
                    throw new RepositoryException(this, e);
                }
        }
        return obj;
    }

    protected RecordUpdate localUpdate(PRIMARY_KEY pk) {
        return new RecordUpdate(this, CRUDE.UPDATE, pk != null ? getCells(pk, true) : null);
    }

    JObject _buildWebApiUpdateJson(Collection<Record> records) {
        JObject json = new JObject()
                .objectC(getKey());

        json.put("lastUpdated", lastUpdate != null ? lastUpdate.getTime() : null);
        json.put("lastUpdatedBy", lastUpdatedBy);
        json.put("updates", updatesCount.get());

        JArray jrows = json.arrayC("rows");
        for (Record rec : records) {
            JObject obj = jrows.object();
            //  obj.put("#crude", rec.crude.name);

            // dla operacji DELETE zwróć tylko ID obiektu
            if (rec.crude == CRUDE.DELETE) {
                obj.put(config.primaryKey.config.key, rec.getPrimaryKeyValue());
                continue;
            }

            for (Column<?> col : rec)
                if (config.primaryKey == col || rec.isChanged(col))
                    obj.put(col.config.key, rec.get(col));
        }
        return json;
    }
}

package com.model.repository;

import com.model.repository.intf.CRUDE;
import com.intf.callable.Callable1;
import com.intf.runnable.Runnable1;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.servlet.websocket.WebSocketConnection;
import com.utils.Utils;
import com.utils.collections.*;
import com.utils.date.TDate;
import com.webapi.core.WebApiController;
import java.lang.reflect.Field;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;

public class Repository<PRIMARY_KEY> {

    public final static SyncMap<String, Repository<?>> ALL = new SyncMap<>();

    public final RepoConfig config = new RepoConfig();

    private final AtomicInteger hash = new AtomicInteger(0); // identator zmiany

    protected final Pairs<Column<?>, Boolean> orderColumns = new Pairs<>(); // kolumna, ASC

    private final HashMap<PRIMARY_KEY, Object[]> records = new HashMap<>();
    final Map<String, Column<?>> columns = new LinkedHashMap<>();
    // ------------ statystyki ------------
    TDate lastUpdate;
    final AtomicInteger updatesCount = new AtomicInteger();
    String lastUpdatedBy;

    public class RepoConfig {

        public String key;
        public CharSequence name;
        public String tableName;
        public Column<?> primaryKey;
        public Boolean readOnly;
        public Boolean autoUpdate; //czy informaje o zmianach mają być automatycznie rozsyłane do klientów
        public DAO dao;
        public Boolean local;

        public void validate() {
            Objects.requireNonNull(key, Repository.this.getClass() + ": key");
            Objects.requireNonNull(name, Repository.this.getClass() + ": name");
            Objects.requireNonNull(primaryKey, Repository.this.getClass() + ": primaryKey");
        }
    }

    //ToDo: statyczna definicja pól
    public Repository(Runnable1<RepoConfig> cfg) {
        cfg.run(config);
        config.validate();

        for (Field f : getClass().getDeclaredFields())
            if (Column.class.isAssignableFrom(f.getType()))
                try {
                    f.setAccessible(true);
                    Column<?> col = (Column<?>) f.get(this);
                    if (columns.containsKey(col.getKey()))
                        throw new RepositoryException(this, "Repozytorium zawiera już kolumnę " + col.getKey());
                    columns.put(col.getKey(), col);
                } catch (Throwable e) {
                    throw new RepositoryException(this, e);
                }
    }

    protected DAOQuery fillQuery(DAOQuery qry) {
        switch (qry.crude) {
            case READ:
                qry.source(config.tableName);

                for (Column<?> col : columns.values())
                    qry.field(col.config.daoName);

                for (Pair<Column<?>, Boolean> pair : orderColumns)
                    qry.order(pair.first.config.daoName, pair.second);

                return qry;
            default:
                throw new UnsupportedOperationException(qry.crude.name());
        }
    }

    protected void fillRecord(Record rec, DAORow row) {

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

    private Object[] getCells(PRIMARY_KEY pk, boolean clone) {
        Object[] cells;
        synchronized (records) {
            cells = records.get(pk);
        }
        if (cells == null)
            throw new RepositoryException(this, "Nie znaleziono rekordu " + Utils.escape(pk));
        return clone ? cells.clone() : cells;
    }

    public Record createOrUpdate(PRIMARY_KEY pk) {
        if (pk == null || !records.containsKey(pk))
            return create();
        else
            return update(pk);
    }

    public Record create() {
        return new Record(this, CRUDE.CREATE, new Object[columns.size()]);
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

    public RepoUpdate<Repository<PRIMARY_KEY>, PRIMARY_KEY> beginUpdate() {
        return new RepoUpdate<>(this);
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

        TList<Record> local = new TList<>();

        MapList<Repository<?>, Record> repos = new MapList<>();

        MapList<DAO, Record> daoRecords = new MapList<>();

        for (Record rec : records) {
            if (rec.changed.isEmpty())
                continue;
            repos.add(rec.repo, rec);
            if (rec.repo.config.dao != null)
                daoRecords.add(rec.repo.config.dao, rec);
            else
                local.add(rec);
        }

        for (Entry<DAO, LinkedList<Record>> en : daoRecords) {
            DAO<?> dao = en.getKey();

            TList<DAOQuery> queries = new TList<>();
            for (Record rec : en.getValue()) {
                DAOQuery qry = new DAOQuery(rec, dao, rec.crude);
                rec.repo.fillQuery(qry);
                queries.add(qry);
            }
            TList<? extends DAORows<?>> results = dao.process(queries);

            // przetwarzanie odpowiedzi
            for (DAORows<?> rows : results) {
                Record rec = (Record) rows.context;
                for (DAORow row : rows) {
                    rec.repo.fillRecord(rec, row);
                    local.add(rec);
                }
            }
        }

        // zakładamy, że operacja się powiodła, aktualizujemy loklane repozytoria
        for (Record rec : local)
            rec.repo.updateRecord(rec);

        // --------- aktualizacja statystyk ------------------
        for (Repository<?> repo : repos.keySet()) {
            repo.lastUpdate = new TDate();
            repo.lastUpdatedBy = "root";
            synchronized (repo.updatesCount) {
                repo.updatesCount.incrementAndGet();
            }
        }

        // roześlij zdarzenie informujące o zmianach w repozytorium do zainteresowanych klientów WebApi
        LinkedList<WebApiController> clients = WebSocketConnection.getControllers(WebApiController.class);
        if (clients.isEmpty())
            return;

        for (Entry<Repository<?>, LinkedList<Record>> en : repos) {
            Repository<?> repo = en.getKey();
            TList<WebApiController> recipients = new TList<>();
            for (WebApiController ctrl : clients)
                if (ctrl.repositories.contains(repo))
                    recipients.add(ctrl);

            if (recipients.isEmpty())
                continue;

            JObject json = new JObject()
                    .objectC(repo.getKey());

            json.put("lastUpdated", repo.lastUpdate != null ? repo.lastUpdate.toString(true) : null);
            json.put("lastUpdatedBy", repo.lastUpdatedBy);
            json.put("updates", repo.updatesCount.get());

            JArray jrows = json.arrayC("rows");
            for (Record rec : en.getValue()) {
                JObject obj = jrows.object();
                obj.put("#crude", rec.crude.name);

                // dla operacji DELETE zwróć tylko ID obiektu
                if (rec.crude == CRUDE.DELETE) {
                    obj.put(repo.config.primaryKey.config.key, rec.getPrimaryKeyValue());
                    continue;
                }

                for (Column<?> col : rec)
                    if (repo.config.primaryKey == col || rec.isChanged(col))
                        obj.put(col.config.key, rec.get(col));
            }

            WebApiController.broadcast("repository", "update", json.getParent(), recipients);
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

            for (Column<?> field : columns.values()) {
                Object value = row.getValue(field.config.daoName, null);
                rec.setAny(field, value);
            }
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
            if (Boolean.TRUE.equals(repo.config.local))
                continue;
            repo.config.local = false;
            repo.config.dao = dao;
            queries.add(repo.fillQuery(new DAOQuery(repo, dao, CRUDE.READ)));
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
        if (includeMetaData) {
            obj.put("key", config.key);
            obj.put("name", config.name);
            obj.put("readOnly", config.readOnly);
            obj.put("pk", config.primaryKey.getKey());
            obj.put("local", config.local);
            obj.put("autoUpdate", config.autoUpdate);
            obj.put("rowsCount", records.size());
            obj.put("lastUpdated", lastUpdate != null ? lastUpdate.toString(true) : null);
            obj.put("lastUpdatedBy", lastUpdatedBy);
            obj.put("updates", updatesCount.get());

            JArray jcol = obj.arrayC("columns");
            for (Column<?> column : columns.values())
                jcol.add(column.getJson());
        }
        if (!includeContent)
            return obj;

        JArray jrows = obj.arrayC("rows");

        TList<Object[]> data = new TList<>();
        synchronized (records) {
            data = new TList<>(records.values());
        }

        for (Object[] row : data)
            jrows.addE(row).asCollection().options.singleLine(true);

        return obj;
    }

}

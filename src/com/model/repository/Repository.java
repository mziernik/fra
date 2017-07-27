package com.model.repository;

import com.events.Dispatcher;
import com.model.repository.intf.CRUDE;
import com.intf.callable.Callable1;
import com.intf.callable.CallableEx1;
import com.intf.runnable.*;
import com.json.JArray;
import com.json.JObject;
import com.json.JSON;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.resources.FontAwesome;
import com.utils.Is;
import com.utils.TObject;
import com.utils.Utils;
import com.utils.collections.*;
import com.utils.date.TDate;
import com.utils.reflections.TField;
import com.webapi.core.WebApiController;
import com.webapi.core.WebApiRequest;
import com.webapi.core.client.Repositories;
import java.lang.reflect.Field;
import java.util.*;
import java.util.Map.Entry;
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
    public final RepoStatus status = new RepoStatus();

    public static enum ActionType {
        BASIC, DEFAULT, PRIMARY, SUCCESS, INFO, WARNING, DANGER
    }

    public class RepoStatus {

        TDate lastUpdate;
        final AtomicInteger revision = new AtomicInteger();
        String lastUpdatedBy;

        private void increment() {
            lastUpdate = new TDate();
            lastUpdatedBy = "root";
            synchronized (revision) {
                revision.incrementAndGet();
            }
        }

        public TDate getLastUpdate() {
            return lastUpdate;
        }

        public String getLastUpdatedBy() {
            return lastUpdatedBy;
        }

        public int getRevision() {
            return revision.get();
        }

        void fillJson(JObject json) {
            json.put("lastUpdated", lastUpdate != null ? lastUpdate.getTime() : null);
            json.put("lastUpdatedBy", lastUpdatedBy);
            json.put("updates", revision.get());
        }
    }

    public class ActionData {

        public final RepoAction action;
        public final WebApiRequest request;
        public final Repository<?> repository = Repository.this;
        public final Record record;
        public final JObject params;
        public final LinkedHashSet<Record> records = new LinkedHashSet<>();

        private ActionData(RepoAction action, WebApiRequest request, Record record, JObject params) {
            this.action = action;
            this.request = request;
            this.record = record;
            this.params = params;
        }

    }

    public class RepoAction {

        public String key;
        public String name;
        public ActionType type;
        public FontAwesome icon;
        public String confirm;
        public boolean record = false;
        public CallableEx1<Object, ActionData> onExecute;

        public RepoAction(boolean record, String key, String name, ActionType type,
                FontAwesome icon, String confirm, CallableEx1<Object, ActionData> onExecute) {
            this.key = key;
            this.name = name;
            this.type = type;
            this.icon = icon;
            this.confirm = confirm;
            this.onExecute = onExecute;
            this.record = record;
        }

        public JObject execute(WebApiRequest request, Record record, JObject params) throws Exception {
            ActionData data = new ActionData(this, request, record, params);
            Object result = onExecute.run(data);

            if (data.records.isEmpty() && record != null)
                data.records.add(record);

            if (result instanceof Record) {
                data.records.add((Record) result);
                result = null;
            }

            MapList<Repository, Record> map = new MapList<>();

            data.records.forEach((Record r) -> map.add(r.repo, r));
            final JObject jresult = new JObject();
            jresult.put("result", result);
            JObject jrecs = jresult.objectC("repositories");

            map.forEach((Entry<Repository, TList<Record>> en)
                    -> AbstractRepoTransaction.fillJson(jrecs, en.getKey(), en.getValue()));

            return jresult;
        }

    }

    public class RepoReference {

        public final CharSequence name;
        public final Column<PRIMARY_KEY> column;

        public RepoReference(CharSequence name, Column<PRIMARY_KEY> column) {
            this.name = name;
            this.column = column;
        }

    }

    public class RepoConfig {

        public String key;
        public CharSequence name;
        public String daoName;
        public CharSequence group;
        public Column<?> primaryKey;
        public Column<?> parentColumn;
        public Column<?> orderColumn;
        public Column<?> displayName;
        public CharSequence description;
        public final LinkedHashMap<String, String> info = new LinkedHashMap<>();
        public DAO dao;
        public Boolean onDemand;
        public Boolean local;
        public Integer limit;
        public Integer offset;
        public Boolean dynamic;
        public FontAwesome icon;
        public final Flags<CRUDE> crude = CRUDE.flags(CRUDE.CRUD);
        public final LinkedHashMap<String, RepoAction> actions = new LinkedHashMap<>();
        public final MapList<Class<? extends Repository<?>>, RepoReference> references = new MapList<>();

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

        public RepoReference reference(CharSequence name, Column<PRIMARY_KEY> column) {
            RepoReference ref = new RepoReference(name, column);
            references.add(column.config.repository, ref);
            return ref;
        }

        public void view(Column<?>... columns) {

        }

        public RepoConfig repoAction(String key, String name, ActionType type, FontAwesome icon,
                String confirm, CallableEx1<Object, ActionData> onExecute) {
            if (actions.containsKey(key))
                throw new RepositoryException(Repository.this, "Akcja " + Utils.escape(key) + " już istnieje");
            actions.put(key, new RepoAction(false, key, name, type, icon, confirm, onExecute));
            return this;
        }

        public RepoConfig recordAction(String key, String name, ActionType type, FontAwesome icon,
                String confirm, CallableEx1<Object, ActionData> onExecute) {
            if (actions.containsKey(key))
                throw new RepositoryException(Repository.this, "Akcja " + Utils.escape(key) + " już istnieje");
            actions.put(key, new RepoAction(true, key, name, type, icon, confirm, onExecute));
            return this;
        }

        private Params buildParams() {
            JObject jActions = new JObject();

            getActions().forEach(act -> jActions.objectC(act.key)
                    .put("record", act.record)
                    .put("name", act.name)
                    .put("confirm", act.confirm)
                    .put("type", act.type != null ? act.type.name().toLowerCase() : null)
                    .put("icon", act.icon != null ? act.icon.className : null).options.singleLine(true)
            );

            JObject jRefs = new JObject();
            references.each((Class< ? extends Repository<?>> repoCls, TList<RepoReference> refs) -> {
                JObject jRef = jRefs.objectC(Repository.getF(repoCls).getKey());
                refs.forEach((RepoReference ref) -> jRef.put("name", ref.name)
                        .put("repo", ref.column.repository.getKey())
                        .put("column", ref.column.getKey()));
            });

            return new Params()
                    .extra("actions", jActions.isEmpty() ? null : jActions)
                    .extra("references", jRefs.isEmpty() ? null : jRefs)
                    .escape("key", key)
                    .escape("name", name)
                    .escape("group", group)
                    .escape("description", description)
                    .add("record", Repository.this.getClass().getSimpleName() + "Record")
                    .escape("primaryKeyColumn", primaryKey.getKey())
                    .escape("parentColumn", parentColumn != null ? parentColumn.getKey() : null)
                    .escape("orderColumn", orderColumn != null ? orderColumn.getKey() : null)
                    .escape("displayNameColumn", displayName != null ? displayName.getKey() : null)
                    .escape("crude", crude.getChars())
                    .add("local", local)
                    .escape("icon", icon != null ? icon.className : null)
                    .add("onDemand", onDemand)
                    .add("info", info.isEmpty() ? null : JSON.serialize(info));
        }

        public Params getParams() {
            Params params = buildParams();
            Is.notNullV(params.extra.get("actions"), obj -> {
                JObject json = (JObject) obj;
                json.options.quotaNames(false);
                params.add("actions", json.toString());
            });

            Is.notNullV(params.extra.get("references"), obj -> {
                JObject json = (JObject) obj;
                json.options.quotaNames(false);
                params.add("references", json.toString());
            });

            return params;
        }

        public void getJson(JObject obj, boolean metaData) {

            if (metaData) {
                Params params = buildParams();
                params.forEach(p -> obj.put(p.name, p.raw));
                Is.notNullV(params.extra.get("actions"), a -> obj.put("actions", a));
                Is.notNullV(params.extra.get("references"), a -> obj.put("references", a));

                Map<String, String> info = new LinkedHashMap<>(this.info);
                info.put("ClassName", getClass().getCanonicalName());

                if (!info.isEmpty())
                    obj._put("info", info).asObject().options.quotaNames(true);

                obj.put("crude", new Strings().map(getCRUDE(), cr -> "" + cr.name().charAt(0)).toString(""));

            }

            obj.put("rowsCount", records.size());
            status.fillJson(obj);

            JArray jcol = obj.arrayC("columns");
            jcol.options.singleLine(true);
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

    /**
     * Zwraca listę uprawnień. Metoda do przeciążenia
     *
     * @return
     */
    protected Flags<CRUDE> getCRUDE() {
        return this.config.crude;
    }

    protected Collection<RepoAction> getActions() {
        return this.config.actions.values();
    }

    public int size() {
        return records.size();
    }

    public boolean has(PRIMARY_KEY pk) {
        return records.containsKey(pk);
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

                    if (qry.crude == CRUDE.UPDATE && !rec.isChanged(col))
                        continue;

                    Object value = null;
                    if (col.config.daoSerializer != null)
                        try {
                            ((CallableEx1<Object, Object>) col.config.daoSerializer).run(rec.get(col));
                        } catch (Error | RuntimeException e) {
                            throw e;
                        } catch (Throwable e) {
                            throw new RepositoryException(this, e).column(col);
                        }
                    else
                        value = rec.serialize(col);

                    if (value == null && Boolean.TRUE.equals(col.config.autoGenerated))
                        continue;

                    String name = col.config.daoName;
                    qry.param(name, col.config.type, value, col.config.daoType);
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

    public PRIMARY_KEY formatPrimaryKey(Object pk) {
        return pk == null || config.primaryKey.config.type.clazz.isAssignableFrom(pk.getClass())
                ? (PRIMARY_KEY) pk : (PRIMARY_KEY) config.primaryKey.config.type.parse(pk);
    }

    Object[] getCells(PRIMARY_KEY pk, boolean clone) {

        Object[] cells;
        if (pk == null)
            throw new RepositoryException(this, "Brak klucza głównego");
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

    public <T extends Number> T min(Column<T> column) {
        return Utils.coalesce(min(column), null);
    }

    public <T extends Number> T min(Column<T> column, T initValue) {
        final TObject<T> min = new TObject<>(initValue);
        forEach(column, (T val) -> {
            if (val != null && (min.isNull() || val.longValue() < min.get().longValue()))
                min.set(val);
            return true;
        });
        return min.get();
    }

    public <T extends Number> T max(Column<T> column, T initValue) {
        final TObject<T> max = new TObject<>(initValue);
        forEach(column, (T val) -> {
            if (val != null && (max.isNull() || val.longValue() > max.get().longValue()))
                max.set(val);
            return true;
        });
        return max.get();
    }

    public <T extends Number> T max(Column<T> column) {
        return Utils.coalesce(max(column), null);
    }

    public <T> void forEach(Column<T> column, Callable1<Boolean, T> consumer) {
        TList<Object[]> rows;

        int idx = new TList(columns.values()).indexOf(column);

        if (idx < 0)
            throw new RepositoryException(this, Utils.frmt("Repozytorium %1 nie posiada kolumny %2",
                    getKey(), column.getKey()));

        synchronized (records) {
            rows = new TList<>(this.records.values());
        }

        for (Object[] cells : rows)
            if (Boolean.FALSE.equals(consumer.run((T) cells[idx])))
                return;
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
    protected void updateRecord(Record record, boolean incrementCounter) {
        if (record.repo != this)
            throw new RepositoryException(this, "Rekord nie należy do repozytorium");

        PRIMARY_KEY pk = (PRIMARY_KEY) record.getPrimaryKeyValue();
        if (pk == null)
            throw new RepositoryException(this, "Piusta wartpość klucza głownego");

        if (record.crude == CRUDE.CREATE || record.crude == CRUDE.UPDATE)
            record.validate();

        synchronized (records) {
            switch (record.crude) {
                case CREATE:
                    if (records.containsKey(pk))
                        throw new RepositoryException(this, "Rekord " + record + " już istnieje");
                    records.put(pk, record.cells.clone());
                    break;

                case UPDATE:
                    Object[] cells = getCells(pk, false);
                    for (int i = 0; i < cells.length; i++)
                        cells[i] = record.cells[i];
                    break;

                case DELETE:
                    Object[] cc = records.remove(pk);
                    if (cc == null)
                        throw new RepositoryException(this, "Nie znaleziono rekordu " + Utils.escape(pk));
                    break;

                default:
                    throw new RepositoryException(this, new UnsupportedOperationException(record.crude.name()));
            }
        }

        if (incrementCounter)
            status.increment();

    }

    static Repository<?> getF(String name) {
        Repository<?> repo = ALL.get(name);
        if (repo == null)
            throw new RepositoryException(null, "Nie znaleziono repozytorium " + Utils.escape(name));
        return repo;
    }

    static Repository<?> getF(Class<? extends Repository<?>> repoClass) {
        Repository<?> repo = ALL.values().findFirst(r -> r.getClass() == repoClass);
        if (repo == null)
            throw new RepositoryException(null, "Nie znaleziono repozytorium " + repoClass.getName());
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
            if (Boolean.TRUE.equals(repo.config.local)
                    || Boolean.FALSE.equals(repo.config.onDemand)
                    || repo.config.daoName == null)
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

    public RepoTransaction<PRIMARY_KEY> beginTransaction() {
        return new RepoTransaction<>(this);
    }

    JObject _buildWebApiUpdateJson(Collection<Record> records) {
        JObject json = new JObject()
                .objectC(getKey());

        status.fillJson(json);

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

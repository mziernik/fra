package com.model.repository;

import com.context.AppContext;
import com.json.JArray;
import com.json.JObject;
import com.mlogger.Log;
import com.model.RRepoHistory;
import com.model.RRepoState;
import com.model.dao.MapDAO;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.model.repository.Repository.RepoReference;
import com.model.repository.intf.CRUDE;
import com.servlet.websocket.WebSocketConnection;
import com.utils.Is;
import com.utils.Utils;
import com.utils.Utils.Visitor;
import com.utils.collections.MapList;
import com.utils.collections.TList;
import com.webapi.core.WebApiController;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

public class AbstractRepoTransaction {

    public final TList<Record> records = new TList<>();
    private ReposTransaction pre;
    private ReposTransaction post;
    Record parent; // dla pre lub post

    protected AbstractRepoTransaction() {

    }

    public ReposTransaction pre() {
        return pre != null ? pre : (pre = new ReposTransaction());
    }

    public ReposTransaction post() {
        return post != null ? post : (post = new ReposTransaction());
    }

    protected <PK> Record createOrUpdate(Repository<PK> repo, PK pk) {
        if (pk == null || !repo.records.containsKey(pk))
            return create(repo);
        else
            return update(repo, pk);
    }

    /**
     * Zwraca rekord dodany wcześniej do transakcji na podstawie klucza głównego
     *
     * @param <PK>
     * @param repo
     * @param pk
     * @return
     */
    protected <PK> Record getRecord(Repository<PK> repo, PK pk, boolean mustExists) {
        Record rec = Utils.findFirst(records, (Record r) -> r.repo == repo
                && Objects.equals(pk, r.getPrimaryKeyValue()));

        if (mustExists && rec == null)
            throw new RepositoryException(repo, "Nie znaleziono rekordu " + Utils.escape(pk));
        return rec;
    }

    protected <PK> Record create(Repository<PK> repo) {
        return records.addR(new Record(repo, CRUDE.CREATE, new Object[repo.columns.size()]));
    }

    protected <PK> Record update(Repository<PK> repo, PK pk) {
        return records.addR(new Record(repo, CRUDE.UPDATE, repo.getCells(pk, true)));
    }

    protected <PK> Record delete(Repository<PK> repo, PK pk) {
        return records.addR(new Record(repo, CRUDE.DELETE, repo.getCells(pk, false)));
    }

    public TList<Record> commit(boolean strict) throws Exception {

        if (records.isEmpty())
            return new TList<>();

        final TList<DAO> daos = new TList<>();
        final TList<Record> local = new TList<>();
        final MapList<Repository<?>, Record> allRepos = new MapList<>();
        final AbstractRepoTransaction history = new AbstractRepoTransaction();

        // usuń wszystkie rekordy zależne (referencje)
        for (Record rec : records) {
            if (rec.crude != CRUDE.DELETE)
                continue;

            Object pk = rec.getPrimaryKeyValue();

            for (RepoReference<?> rr : rec.repo.config.references.values()) {
                Repository<?> repo = rr.column.repository;
                repo.forEach(r -> {
                    if (Objects.equals(pk, r.get(rr.column)))
                        pre().delete((Repository) repo, r.getPrimaryKeyValue());
                    return true;
                });
            }
        }

        Utils.visit(this, (AbstractRepoTransaction transaction, Visitor<AbstractRepoTransaction> visitor) -> {

            if (transaction.pre != null)
                visitor.visit(pre, visitor);

            MapList<DAO, Record> daoRecords = new MapList<>();
            final MapList<Repository<?>, Record> repos = new MapList<>();

            for (Record rec : transaction.records) {
                if (rec.crude != CRUDE.DELETE && rec.changed.isEmpty())
                    continue;
                repos.add(rec.repo, rec);

                if (rec.repo.config.dao != null)
                    daoRecords.add(rec.repo.config.dao, rec);
                else
                    local.add(rec);
            }

            for (Repository<?> repo : new TList<>(repos.keySet()))
                if (!repo.beforeCommit(repos.get(repo)))
                    repos.remove(repo);

            allRepos.addAll(repos);

            repos.entrySet().forEach((en) -> {
                en.getKey().onBeforeUpdate.dispatch(this, intf -> intf.run(en.getValue(), repos));
            });

            for (Entry<DAO, TList<Record>> en : daoRecords) {
                DAO<?> dao = en.getKey();
                daos.add(dao);

                if (dao.isTransactional() && !dao.inTransaction())
                    dao.beginTransaction();

                TList<DAOQuery> queries = new TList<>();
                for (Record rec : en.getValue()) {
                    DAOQuery qry = new DAOQuery(rec, dao, rec.crude);
                    rec.repo.fillQuery(qry, rec, transaction.parent);
                    queries.add(qry);
                }
                TList<? extends DAORows<?>> results = dao.process(queries);

                // przetwarzanie odpowiedzi
                for (DAORows<?> rows : results) {
                    Record rec = (Record) rows.context;

                    if (rows.size() > 1)
                        throw new RepositoryException(rec.repo, Utils.frmt("Zbyt dużo wyników (%1)", results.size()));

                    if (rows.crude == CRUDE.UPDATE && rows.isEmpty()) {
                        Log.warning("Repository", "Zapytanie UPDATE nie zwróciło rezultatu, wymuszam INSERT");

                        DAOQuery query = new DAOQuery(rows.context, dao, CRUDE.CREATE);
                        rec.repo.fillQuery(query, rec, transaction.parent);
                        rows = dao.process(query);
                    }

                    for (DAORow row : rows) {
                        rec.repo.fillRecord(rec, row);
                        local.add(rec);
                    }

                    if (!(rec.repo instanceof RRepoHistory) && RRepoHistory.instance != null)
                        RRepoHistory.instance.fill(history.create(RRepoHistory.instance), rec);
                }
            }

            if (transaction.post != null)
                visitor.visit(post, visitor);

        });

        if (!history.records.isEmpty())
            history.commit(true);

        for (DAO dao : daos)
            if (dao.isTransactional() && dao.inTransaction())
                dao.commitTransaction();

        // zakładamy, że operacja się powiodła, aktualizujemy loklane repozytoria
        for (Record rec : local)
            rec.repo.updateRecord(rec, true);

        webApiBroadcast(allRepos);
        return local;
    }

    private static void clearChangedFlag(Record rec) {
        if (!(rec instanceof RecordUpdate))
            return;

        for (Column<?> col : rec)
            rec.changed.remove(col);
    }

    static void webApiBroadcast(Record rec) {
        if (!AppContext.isInitialized())
            return;
        MapList<Repository<?>, Record> repos = new MapList<>();
        repos.add(rec.repo, rec);
        webApiBroadcast(repos);
    }

    public static void fillJson(JObject _json, Repository<?> repo, TList<Record> records) {

        JObject json = _json.objectC(repo.getKey());

        repo.status.fillJson(json);

        JArray jrows = json.arrayC("rows");
        for (Record rec : records) {
            JObject obj = jrows.object();
            //  obj.put("#crude", rec.crude.name);

            // dla operacji DELETE zwróć tylko ID obiektu
            if (rec.crude == CRUDE.DELETE) {
                json.arrayC("deletedRows").add(rec.getPrimaryKeyValue());
                continue;
            }

            for (Column<?> col : rec)
                if (repo.config.primaryKey == col || rec.isChanged(col))
                    obj.put(col.config.key, rec.serialize(col));

            clearChangedFlag(rec);
        }
    }

    static void webApiBroadcast(MapList<Repository<?>, Record> repos) {
        if (!AppContext.isInitialized())
            return;
        // roześlij zdarzenie informujące o zmianach w repozytorium do zainteresowanych klientów WebApi
        TList<WebApiController> clients = WebSocketConnection.getControllers(WebApiController.class);
        if (clients.isEmpty()) {
            repos.allValues().forEach(AbstractRepoTransaction::clearChangedFlag);
            return;
        }

        for (Entry<Repository<?>, TList<Record>> en : repos) {
            Repository<?> repo = en.getKey();

            if (!RRepoState.canBroadcast(repo))
                continue;

            TList<WebApiController> recipients = new TList<>();
            for (WebApiController ctrl : clients)
                if (ctrl.repositories.contains(repo))
                    recipients.add(ctrl);

            if (recipients.isEmpty() || !repo.beforeBroadcast(recipients))
                continue;

            if (recipients.isEmpty())
                continue;

            JObject json = new JObject();
            fillJson(json, repo, en.getValue());

            WebApiController.broadcast("repository", "update", json, recipients);
        }

    }

    protected <PK> Record action(Repository<PK> repo, CRUDE crude, DAO dao) throws Exception {

        DAOQuery query = new DAOQuery(this, dao, crude);

        query.field(repo.config.primaryKey.getKey());

        DAORows drows = dao.process(query);
        if (drows.isEmpty())
            throw new RepositoryException(repo, "Brak wyników");
        if (drows.size() > 1)
            throw new RepositoryException(repo, "Zbyt wiele wyników (" + drows.size());

        DAORow row = drows.first();

        String pkName = repo.config.primaryKey.getKey();
        PK pk = Is.notNullR(row.getValue(pkName, null),
                pkVal -> (PK) repo.config.primaryKey.config.type.parse(pkVal));

        if (crude == null && row.contains("#action"))
            crude = CRUDE.get(Utils.toString(row.getValue("#action", null)));

        if (crude == null) {

            if (!row.contains(pkName))
                crude = CRUDE.CREATE;

            if (crude == null)
                if (row.getNames().size() == 1)
                    crude = CRUDE.DELETE;
                else
                    crude = repo.records.containsKey(pk) ? CRUDE.UPDATE : CRUDE.CREATE;
        };

        Record rec = new Record(repo, crude, new Object[repo.columns.size()]);
        if (pk != null)
            rec.cells = repo.getCells(pk, true);

        for (String s : row.getNames()) {
            if (s.equals("#action") || s.equals("#uid"))
                continue;

            if (s.equals("#refs")) {
                Map<String, List<Map<String, Object>>> map = (Map) row.getValue(s, null);
                ReposTransaction post = post();
                post.parent = rec;

                Utils.forEach(map, (String name, List<Map<String, Object>> objects) -> {
                    Repository r = Repository.getF(name);
                    for (Map obj : objects)
                        post.action(r, null, new MapDAO(obj));
                });

//                for (JArray arr : json.getArrays()) {
//                    Repository repo = Repository.getF(arr.getName());
//                    for (JObject obj : arr.getObjects())
//                        trans.action(repo, null, new MapDAO(obj));
//                }
                continue;
            }

            Column<?> col = repo.columns.get(s);
            if (col == null) {
                Log.warning("Repository", "Nie znaleziono kolumny " + Utils.escape(s));
                continue;
            }
            Object val = row.getValue(s, null);
            if (val == null && Boolean.TRUE.equals(col.config.autoGenerated))
                continue;
            rec.setAny(col, val);
        }

        return records.addR(rec);

    }

    /**
     * Wczytaj wszystkie rekordy bez powiadamiania o zmianach
     */
    public void loadAll() {
        for (Record rec : records)
            rec.repo.updateRecord(rec, false);
    }
}

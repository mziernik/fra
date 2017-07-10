package com.model.repository;

import com.json.JArray;
import com.json.JObject;
import com.mlogger.Log;
import com.model.RRepoHistory;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.model.repository.intf.CRUDE;
import com.servlet.websocket.WebSocketConnection;
import com.utils.Utils;
import com.utils.collections.MapList;
import com.utils.collections.TList;
import com.utils.date.TDate;
import com.webapi.core.WebApiController;
import java.util.Map.Entry;

public class RepoTransaction {

    public final TList<Record> records = new TList<>();

    public <PK> Record createOrUpdate(Repository<PK> repo, PK pk) {
        if (pk == null || !repo.records.containsKey(pk))
            return create(repo);
        else
            return update(repo, pk);
    }

    public <PK> Record create(Repository<PK> repo) {
        return records.addR(new Record(repo, CRUDE.CREATE, new Object[repo.columns.size()]));
    }

    public <PK> Record update(Repository<PK> repo, PK pk) {
        return records.addR(new Record(repo, CRUDE.UPDATE, repo.getCells(pk, true)));
    }

    public <PK> Record delete(Repository<PK> repo, PK pk) {
        return records.addR(new Record(repo, CRUDE.DELETE, repo.getCells(pk, false)));
    }

    public void commit(boolean strict) throws Exception {

        if (records.isEmpty())
            return;

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

        for (Repository<?> repo : new TList<>(repos.keySet()))
            if (!repo.beforeCommit(repos.get(repo)))
                repos.remove(repo);

        for (Entry<Repository<?>, TList<Record>> en : repos.entrySet())
            en.getKey().onBeforeUpdate.dispatch(this, intf -> intf.run(en.getValue(), repos));

        RepoTransaction history = new RepoTransaction();

        for (Entry<DAO, TList<Record>> en : daoRecords) {
            DAO<?> dao = en.getKey();

            TList<DAOQuery> queries = new TList<>();
            for (Record rec : en.getValue()) {
                DAOQuery qry = new DAOQuery(rec, dao, rec.crude);
                rec.repo.fillQuery(qry, rec);
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
                if (!(rec.repo instanceof RRepoHistory) && RRepoHistory.instance != null)
                    RRepoHistory.instance.fill(history.create(RRepoHistory.instance), rec);

            }
        }

        if (!history.records.isEmpty())
            history.commit(true);

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
        webApiBroadcast(repos);
    }

    private static void clearChangedFlag(Record rec) {
        if (!(rec instanceof RecordUpdate))
            return;

        for (Column<?> col : rec)
            rec.changed.remove(col);
    }

    static void webApiBroadcast(MapList<Repository<?>, Record> repos) {

        // roześlij zdarzenie informujące o zmianach w repozytorium do zainteresowanych klientów WebApi
        TList<WebApiController> clients = WebSocketConnection.getControllers(WebApiController.class);
        if (clients.isEmpty()) {
            repos.allValues().forEach(RepoTransaction::clearChangedFlag);
            return;
        }

        for (Entry<Repository<?>, TList<Record>> en : repos) {
            Repository<?> repo = en.getKey();

            TList<WebApiController> recipients = new TList<>();
            for (WebApiController ctrl : clients)
                if (ctrl.repositories.contains(repo))
                    recipients.add(ctrl);

            if (recipients.isEmpty() || !repo.beforeBroadcast(recipients))
                continue;

            if (recipients.isEmpty())
                continue;

            JObject json = new JObject()
                    .objectC(repo.getKey());

            json.put("lastUpdated", repo.lastUpdate != null ? repo.lastUpdate.getTime() : null);
            json.put("lastUpdatedBy", repo.lastUpdatedBy);
            json.put("updates", repo.updatesCount.get());

            JArray jrows = json.arrayC("rows");
            for (Record rec : en.getValue()) {
                JObject obj = jrows.object();
                //  obj.put("#crude", rec.crude.name);

                // dla operacji DELETE zwróć tylko ID obiektu
                if (rec.crude == CRUDE.DELETE) {
                    obj.put(repo.config.primaryKey.config.key, rec.getPrimaryKeyValue());
                    continue;
                }

                for (Column<?> col : rec)
                    if (repo.config.primaryKey == col || rec.isChanged(col))
                        obj.put(col.config.key, rec.get(col));

                clearChangedFlag(rec);
            }

            WebApiController.broadcast("repository", "update", json.getParent(), recipients);
        }

    }

    public <PK> Record action(Repository<PK> repo, CRUDE crude, DAO dao) throws Exception {

        DAOQuery query = new DAOQuery(this, dao, crude);

        query.field(repo.config.primaryKey.getKey());

        DAORows drows = dao.process(query);
        if (drows.isEmpty())
            throw new RepositoryException(repo, "Brak wyników");
        if (drows.size() > 1)
            throw new RepositoryException(repo, "Zbyt wiele wyników (" + drows.size());

        DAORow row = drows.first();

        String pkName = repo.config.primaryKey.getKey();
        PK pk = null;
        if (crude == null) {

            if (!row.contains(pkName))
                crude = CRUDE.CREATE;

            if (crude == null) {
                pk = (PK) repo.config.primaryKey.config.type.parse(row.getValue(pkName, null));
                if (row.getNames().size() == 1)
                    crude = CRUDE.DELETE;
                else
                    crude = repo.records.containsKey(pk) ? CRUDE.UPDATE : CRUDE.CREATE;
            }
        } else if (crude != CRUDE.CREATE)
            pk = (PK) repo.config.primaryKey.config.type.parse(row.getValue(pkName, null));

        Record rec = new Record(repo, crude, new Object[repo.columns.size()]);
        if (pk != null)
            rec.cells = repo.getCells(pk, true);

        for (String s : row.getNames()) {
            Column<?> col = repo.columns.get(s);
            if (col == null) {
                Log.warning("Repository", "Nie znaleziono kolumny " + Utils.escape(s));
                continue;
            }
            rec.setAny(col, row.getValue(s, null));
        }

        return records.addR(rec);

    }
}

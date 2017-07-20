package com.model.repository;

import com.cache.CachedData;
import com.json.JArray;
import com.json.JCollection;
import com.json.JObject;
import com.json.JValue;
import com.model.dao.MapDAO;
import com.model.repository.Repository.RepoAction;
import com.servlet.interfaces.Arg;
import com.servlet.websocket.WebSocketController;
import com.utils.Utils;
import com.utils.collections.MapList;
import com.utils.collections.TList;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map.Entry;

public class WRepository implements WebApi {

    @WebApiEndpoint(description = "Lista wszystkich rekordów w cache")
    public JObject list() {
        JObject json = new JObject();

        for (Repository<?> repo : Repository.ALL.values())
            json.put(repo.getKey(), repo.getJson(true, false));

        return json;
    }

    @WebApiEndpoint(description = "Zwraca dane z wielu tabel")
    public JObject get(WebApiRequest req,
            @Arg(name = "repositories", required = false) JCollection repositories)
            throws FileNotFoundException {

        //ToDo: sparametryzować czy mają być zwracane meta dane czy tylko wiersze
        TList<Repository<?>> repos = new TList<>();
        if (repositories != null && !repositories.isEmpty())
            for (JValue val : repositories.getValues()) {
                Repository<?> repo = Repository.getF(
                        repositories.isArray() ? val.asString() : val.getName());
                final WebSocketController ws = req.webSocket;
                //    if (val.isBoolean() && val.asBoolean() && ws != null)
                req.controller.repositories.add(repo);

                repos.add(repo);
            }
        else
            repos.addAll(Repository.ALL.values());

        JObject json = new JObject();

        for (Repository<?> repo : repos)
            json.put(repo.getKey(), repo.getJson(false, true));

        //tbl.getJson().toString()
        return json;
    }

    @WebApiEndpoint()
    public JObject action(WebApiRequest req,
            @Arg(name = "repo", required = true) String repoKey,
            @Arg(name = "action", required = true) String actionKey,
            @Arg(name = "pk") Object pk,
            @Arg(name = "params") JObject params
    ) throws Exception {
        Repository<?> repo = Repository.getF(repoKey);
        RepoAction act = repo.config.actions.get(actionKey);
        if (act == null)
            throw new RepositoryException(repo, Utils.frmt("Nie znaleziono akcji %1", actionKey));

        Record rec = act.record ? ((Repository) repo).read(repo.formatPrimaryKey(pk)) : null;

        return act.execute(req, rec, params);

    }

    @WebApiEndpoint()
    public JObject edit(WebApiRequest req, @Arg(name = "data") JObject json) throws Exception {

        ReposTransaction trans = new ReposTransaction();

        for (JArray arr : json.getArrays()) {

            Repository repo = Repository.getF(arr.getName());

            for (JObject obj : arr.getObjects()) {
                MapDAO dao = new MapDAO(obj);
                trans.action(repo, null, dao);
            }
        }

        TList<Record> records = trans.commit(true);

        MapList<Repository, Record> map = new MapList<>();
        records.forEach(rec -> map.add(rec.repo, rec));

        final JObject result = new JObject();
        map.forEach((Entry<Repository, TList<Record>> en)
                -> AbstractRepoTransaction.fillJson(result, en.getKey(), en.getValue()));
        return result;
    }

    @WebApiEndpoint
    public CachedData export() throws IOException {
        /*
        JObject json = new JObject();

        for (Entry<String, Repository_old<?, ?>> en : Repository_old.getTables().entrySet())
            json.put(en.getKey(), en.getValue().getJson().array("rows"));

        CachedData cd = new CachedData("aaa", "xxx", "plik.json");
        json.write(cd);
        cd.close();

        return cd;
         */
        return null;
    }
}

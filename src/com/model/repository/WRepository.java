package com.model.repository;

import com.cache.CachedData;
import com.json.JArray;

import com.json.JObject;
import com.json.JValue;
import com.model.dao.MapDAO;
import com.model.repository.intf.CRUDE;
import com.servlet.interfaces.Arg;
import com.servlet.websocket.WebSocketController;
import com.utils.collections.TList;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;
import java.io.FileNotFoundException;
import java.io.IOException;

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
            @Arg(name = "repositories", required = false) JObject repositories)
            throws FileNotFoundException {

        //ToDo: sparametryzować czy mają być zwracane meta dane czy tylko wiersze
        TList<Repository<?>> repos = new TList<>();
        if (repositories != null && !repositories.isEmpty())
            for (JValue val : repositories.getValues()) {
                Repository<?> repo = Repository.getF(val.getName());
                final WebSocketController ws = req.webSocket;
                if (val.isBoolean() && val.asBoolean() && ws != null)
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
    public JObject edit(WebApiRequest req, @Arg(name = "data") JObject json) throws Exception {

        final JObject result = new JObject();

        RepoTransaction trans = new RepoTransaction();

        for (JArray arr : json.getArrays()) {

            Repository repo = Repository.getF(arr.getName());

            for (JObject obj : arr.getObjects()) {
                CRUDE crude = CRUDE.get(obj.getStr("action"));
                MapDAO dao = new MapDAO(obj.objectF("fields"));
                trans.action(repo, crude, dao);
            }
        }

        trans.commit(true);

        return null;
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

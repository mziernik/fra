package com.database.model;

import com.cache.CachedData;
import com.intf.runnable.RunnableEx;
import com.json.JArray;
import com.json.JObject;
import com.json.JValue;
import com.model.dataset.DsColumn;
import com.model.dataset.intf.CRUDE;
import com.servlet.Handlers;
import com.servlet.interfaces.Arg;
import com.servlet.websocket.WebSocketController;
import com.utils.collections.TList;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map;
import java.util.Map.Entry;

public class WRepository implements WebApi {

    @WebApiEndpoint(description = "Lista wszystkich rekordów w cache")
    public JObject list() {
        JObject json = new JObject();

        for (Repository<?, ?> tbl : Repository.getTables().values()) {
            JObject obj = json.objectC(tbl.key);
            obj.put("title", tbl.name);
            obj.put("rows", tbl.size());
            JObject jcol = obj.objectC("column");
            for (DsColumn<?, ?, ?, ?> col : tbl.getColumns().values())
                jcol.put(col.getKey(), col.getJson());
        }

        return json;
    }

    @WebApiEndpoint(description = "Zwraca dane z wielu tabel")
    public JObject getData(WebApiRequest req,
            @Arg(name = "repositories", required = false) JObject repositories) throws FileNotFoundException {

        TList<Repository<?, ?>> tbls = new TList<>();
        if (repositories != null && !repositories.isEmpty())
            for (JValue val : repositories.getValues()) {
                Repository<?, ?> repo = Repository.getRepoF(val.getName());
                final WebSocketController ws = req.webSocket;
                if (val.isBoolean() && val.asBoolean() && ws != null) {
                    repo.wsConnections.add(ws);
                    ws.onClose.listen(this, cr -> repo.wsConnections.remove(ws));
                }
                tbls.add(repo);
            }
        else
            tbls.addAll(Repository.repos1.values());

        JObject json = new JObject();

        for (Repository<?, ?> tbl : tbls)
            json.put(tbl.key, tbl.getJson());

        //tbl.getJson().toString()
        return json;
    }

    @WebApiEndpoint()
    public JObject edit(WebApiRequest req, @Arg(name = "data") JObject json) throws Exception {

        final JObject result = new JObject();

        DbRecordTransaction trans = new DbRecordTransaction();

        for (JArray arr : json.getArrays()) {

            Repository tbl = Repository.getRepo(arr.getName());
            JArray jRes = result.arrayC(arr.getName());
            for (JObject obj : arr.getObjects()) {
                CRUDE crude = CRUDE.get(obj.getStr("action"));
                Map<String, Object> fields = obj.objectF("fields").asMap();
                Repository t = tbl.action(crude, fields, trans);
                jRes.add(t.getJson());
            }
        }

        trans.commit(Handlers.database.getInstance().getDatabase());
        return result;
    }

    @WebApiEndpoint
    public CachedData export() throws IOException {

        JObject json = new JObject();

        for (Entry<String, Repository<?, ?>> en : Repository.getTables().entrySet())
            json.put(en.getKey(), en.getValue().getJson().array("rows"));

        CachedData cd = new CachedData("aaa", "xxx", "plik.json");
        json.write(cd);
        cd.close();

        return cd;
    }
}

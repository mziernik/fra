package com.database.model;

import com.cache.CachedData;
import com.json.JArray;
import com.json.JObject;
import com.model.dataset.DsColumn;
import com.model.dataset.intf.CRUDE;
import com.servlet.Handlers;
import com.servlet.interfaces.Arg;
import com.utils.collections.TList;
import com.webapi.core.DataType_old;
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

        for (DsTable<?, ?> tbl : DsTable.getTables().values()) {
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
    public JObject getAll(WebApiRequest req,
            @Arg(name = "repositories", required = false) String[] repositories) throws FileNotFoundException {

        TList<DsTable<?, ?>> tbls = new TList<>();
        if (repositories != null && repositories.length > 0)
            for (String s : repositories)
                tbls.add(DsTable.getTableF(s));
        else
            tbls.addAll(DsTable.tables1.values());

        JObject json = new JObject();

        for (DsTable<?, ?> tbl : tbls)
            json.put(tbl.key, tbl.getJson());

        //tbl.getJson().toString()
        return json;
    }

    @WebApiEndpoint(dataType = DataType_old.OBJECT)
    public JObject edit(
            WebApiRequest req,
            @Arg(name = "data") JObject json
    ) throws Exception {

        final JObject result = new JObject();

        DbRecordTransaction trans = new DbRecordTransaction();

        for (JArray arr : json.getArrays()) {

            DsTable tbl = DsTable.getTable(arr.getName());
            JArray jRes = result.arrayC(arr.getName());
            for (JObject obj : arr.getObjects()) {
                CRUDE crude = CRUDE.get(obj.getStr("action"));
                Map<String, Object> fields = obj.objectF("fields").asMap();
                DsTable t = tbl.action(crude, fields, trans);
                jRes.add(t.getJson());
            }
        }

        trans.commit(Handlers.database.getInstance().getDatabase());
        return result;
    }

    @WebApiEndpoint
    public CachedData export() throws IOException {

        JObject json = new JObject();

        for (Entry<String, DsTable<?, ?>> en : DsTable.getTables().entrySet())
            json.put(en.getKey(), en.getValue().getJson().array("rows"));

        CachedData cd = new CachedData("aaa", "xxx", "plik.json");
        json.write(cd);
        cd.close();

        return cd;
    }
}

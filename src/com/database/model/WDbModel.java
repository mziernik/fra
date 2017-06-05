package com.database.model;

import com.cache.CachedData;
import com.json.JArray;
import com.json.JObject;
import com.model.dataset.DsColumn;
import com.servlet.Handlers;
import com.servlet.interfaces.Arg;
import com.utils.collections.TList;
import com.webapi.core.DataType;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import com.webapi.core.WebApiRequest;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map.Entry;

public class WDbModel implements WebApi {

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
            @Arg(name = "repositories") String[] repositories) throws FileNotFoundException {

        TList<DsTable<?, ?>> tbls = new TList<>();
        for (String s : repositories)
            tbls.add(DsTable.getTableF(s));

        JObject json = new JObject();

        for (DsTable<?, ?> tbl : tbls)
            json.put(tbl.key, tbl.getJson());

        //tbl.getJson().toString()
        return json;
    }

    @WebApiEndpoint(dataType = DataType.OBJECT)
    public DsTable<?, ?> edit(
            WebApiRequest req,
            @Arg(name = "table") String table,
            @Arg(name = "key", required = false) Object key
    ) throws Exception {
        DsTable tbl = DsTable.getTable(table);
        DbRecordTransaction trans = new DbRecordTransaction();
        tbl = DsTable.edit(tbl, key, req.getJson().asObject().asMap(), trans);
        trans.commit(Handlers.database.getInstance().getDatabase());
        return tbl;
    }

    @WebApiEndpoint()
    public DsTable<?, ?> remove(
            @Arg(name = "table") String table,
            @Arg(name = "key") Object key
    ) throws Exception {
        DsTable tbl = DsTable.getTable(table);
        DbRecordTransaction trans = new DbRecordTransaction();
        tbl = DsTable.remove(tbl, key, trans);
        trans.commit(Handlers.database.getInstance().getDatabase());
        return tbl;
    }

    @WebApiEndpoint(dataType = DataType.ARRAY)
    public DsTable<?, ?>[] editMultiple(WebApiRequest req) throws Exception {

        TList<JObject> objs = req.getJson().asArray().getObjects();
        if (objs.isEmpty())
            return null;

        TList<DsTable<?, ?>> list = new TList<>();

        DbRecordTransaction trans = new DbRecordTransaction();

        for (JObject obj : objs) {
            DsTable tbl = DsTable.getTable(obj.getStr("table"));
            tbl = DsTable.edit(tbl, obj.getRawValue("key", null),
                    obj.objectF("data").asMap(), trans);
            list.add(tbl);
        }

        trans.commit(Handlers.database.getInstance().getDatabase());
        return list.toArray(new DsTable[0]);
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

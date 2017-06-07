package com.database;

import com.database.elements.*;
import com.json.JObject;
import com.model.dataset.AbstractDataSet;
import com.model.dataset.DataSet;
import com.servlet.interfaces.Arg;
import com.utils.hashes.Hashes;
import com.utils.reflections.DataType;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class WDatabase implements WebApi {

    public final static Map<String, DBConnectionData> databases = new LinkedHashMap<>();

    public static void addDatabase(DBConnectionData connData) {
        databases.put(Hashes.idHash6(connData.toString()), connData);
    }

    @WebApiEndpoint()
    public AbstractDataSet getSessions() {
        return null;
    }

    private Database getDb(String id) throws Exception {
        DBConnectionData conn = databases.get(id);
        if (conn == null)
            throw new Error(String.format("Database %s not found", id));
        Database db = conn.driver.newInstance();
        db.connData = conn;
        return db;
    }

    @WebApiEndpoint()
    public JObject getDatabases() throws Exception {

        JObject json = new JObject();

        for (Entry<String, DBConnectionData> en : databases.entrySet()) {
            DBConnectionData connData = en.getValue();
            JObject obj = json.objectC(en.getKey());
            obj.put("name", en.getValue().toString());
        }

        return json;
    }

    @WebApiEndpoint()
    public JObject getMeta(@Arg(name = "id") String id) throws Exception {
        Database db = getDb(id);

        MetaDbStructure meta = db.meta.getStructure();

        JObject json = new JObject();
        json.put("id", id);
        json.put("name", db.connData.toString());

        JObject jcatalogs = json.objectC("catalog");
        for (MetaCatalog cat : meta.catalogs.values()) {

            JObject jcat = jcatalogs.objectC(cat.name);

            JObject jschemas = jcat.objectC("schema");
            for (MetaSchema sch : cat.schemas.values()) {

                JObject jschema = jschemas.objectC(sch.name)
                        .put("meta", sch.meta);

                {
                    JObject jfunctions = jschema.objectC("function");
                    for (MetaFunction f : sch.functions.values())
                        jfunctions.objectC(f.name)
                                .put("type", f.type);

                }

                JObject jtables = jschema.objectC("table");

                for (MetaTable tbl : sch.tables.values()) {

                    JObject jtable = jtables.objectC(tbl.name)
                            .put("meta", tbl.meta)
                            .put("type", tbl.type);

                    JObject jcolumns = jtable.objectC("column");

                    for (MetaColumn col : tbl.columns.values())
                        jcolumns.objectC(col.name)
                                .put("type", col.type)
                                .put("def", col.defaultValue)
                                .put("pk", col.primaryKey)
                                .put("null", col.nullable)
                                .put("size", col.size)
                                .put("autoIncrement", col.autoIncrement);

                }

            }
        }
        return json;
    }

    @WebApiEndpoint()
    public DataSet execute(
            @Arg(name = "id") String id,
            @Arg(name = "query") String query,
            @Arg(name = "limit", required = false) Integer limit
    ) throws Exception {

        Database db = getDb(id);

        QueryRows rows = db.execute(query);
        DataSet dataSet = new DataSet("db", "Rezultat");

        for (QueryColumn col : rows.columns)
            dataSet.column(String.class, col.name, DataType.STRING, col.name, null)
                    .subtitle(col.type);

        for (QueryRow qrow : rows)
            dataSet.addRow(null, qrow.values);

//            dataSet.addRow().addAll(qrow.values);
        return dataSet;
    }

    @WebApiEndpoint()
    public void invalidateSession(@Arg(name = "id") String id) {

    }
}

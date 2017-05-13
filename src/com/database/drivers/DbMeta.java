package com.database.drivers;

import com.database.Database;
import com.database.elements.MetaCatalog;
import com.database.elements.MetaDbStructure;
import com.database.elements.MetaSchema;
import com.database.elements.MetaTable;
import com.utils.Is;
import com.utils.Utils;
import com.utils.Is;
import java.sql.*;
import java.util.*;

public abstract class DbMeta {

    protected final Database db;

    public DbMeta(Database db) {
        this.db = db;
    }

    public abstract MetaDbStructure getStructure() throws SQLException;

    public abstract Collection<Object> processArray(Object obj) throws SQLException;

    public abstract Map<Object, Object> processMap(Object obj) throws SQLException;

    /**
     * Formatowanie natywnego obiektu do uzytecznej postaci, np PgArray -> List
     *
     * @param obj
     * @return
     */
    public Object formatObject(Object obj) throws SQLException {
        return obj;
    }

    protected MetaCatalog getCatalog(MetaDbStructure result, String name) {
        MetaCatalog cat = result.catalogs.get(name);
        if (cat != null)
            return cat;
        if (name == null)
            return Objects.requireNonNull(result.mainCatalog);
        return new MetaCatalog(result, name);
    }

    protected MetaSchema getSchema(MetaCatalog catalog, String name) {
        MetaSchema sch = catalog.schemas.get(name);
        if (sch != null)
            return sch;
        return new MetaSchema(catalog, Is.in(name.toLowerCase(),
                "information_schema", "pg_catalog", "pg_toast"),
                name);
    }

    protected MetaTable getTable(MetaSchema schema, String name) {
        MetaTable table = schema.tables.get(name);
        if (table != null)
            return table;
        return new MetaTable(schema, schema.meta, name);
    }

}

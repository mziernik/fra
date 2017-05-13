package com.database.orm.generator;

import com.utils.StrUtils;
import com.utils.JavaFile;
import java.io.File;
import java.io.IOException;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

abstract class DTableView {

    final List<DForeignKey> foreignKeys = new LinkedList<>();
    public final Map<String, DColumn> columns = new LinkedHashMap<>();
    final String catalog;
    final DSchema schema;
    final String name;
    final String type;
    final String className;
    final String canonicalName;
    DPrimaryKey pkey;
    final Map<String, DIndex> indexes = new LinkedHashMap<>();
    public final String pckg;

    DTableView(DSchema schema, DatabaseMetaData meta, ResultSet tables) throws SQLException {
        catalog = tables.getString("table_cat");
        name = tables.getString("table_name");
        type = tables.getString("table_type");
        this.schema = schema;

        String pckg = null;
        if (this instanceof DTable) {
            schema.tables.put(name, (DTable) this);
            pckg = schema.pckg + ".table";
        }
        if (this instanceof DView) {
            schema.views.put(name, (DView) this);
            pckg = schema.pckg + ".view";
        }

        this.pckg = pckg;
        String cls = StrUtils.formatMethodName(name);
        cls = cls.substring(0, 1).toUpperCase() + cls.substring(1);

        this.className = cls;
        this.canonicalName = pckg + "." + cls;
    }

    void processIndexes(DatabaseMetaData meta) throws SQLException {
        ResultSet indexInfo = meta.getIndexInfo(null, schema.name, name, false, false);
        while (indexInfo.next()) {
            String idxName = indexInfo.getString("index_name");
            DColumn col = columns.get(indexInfo.getString("column_name"));
            if (col == null)
                continue;
            DIndex idx = indexes.get(idxName);
            if (idx == null) {
                idx = new DIndex(indexInfo);
                indexes.put(idxName, idx);
            }
            idx.columns.add(col);
            idx.colNames.add(col.fieldName);
        }

    }

    abstract void build(JavaFile jSchema, File dir) throws IOException;

}

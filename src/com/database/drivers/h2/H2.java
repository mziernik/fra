package com.database.drivers.h2;

import com.utils.Utils;
import com.utils.Is;
import com.database.DBConnectionData;
import com.database.Database;
import com.database.elements.*;
import com.database.queries.Merge;
import com.database.queries.MergeMultiple;
import com.database.queries.builder.*;
import com.utils.Undefined;
import com.utils.text.StrWriter;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Miłosz Ziernik 2014/06/20
 */
@JdbcDriver("org.h2.Driver")
public class H2 extends Database {

    public final H2Meta meta;

    public H2(DBConnectionData connData) {
        super(connData, H2Meta.class);
        this.meta = (H2Meta) super.meta;
    }

    public H2() {
        super(null, H2Meta.class);
        this.meta = (H2Meta) super.meta;
    }

    @Override
    public Merge merge(String table, String keyColumn) {
        return new Merge(this, table, keyColumn);
    }

    public MergeMultiple mergeMultiple(String table, String keyColumn, String... columns) {
        return new MergeMultiple(this, table, keyColumn, columns);
    }

    /*
     @Override
     public void checkStructure(List<HTable> tables) throws SQLException {

     if (tables == null || tables.isEmpty())
     return;

     DatabaseMetaData meta = getMetaData();
     Strings schemas = this.meta.getSchemas(meta);

     Strings queries = new Strings();
     MetaTables mtables = new MetaTables(this, meta);

     for (HTable table : tables) {

     if (!table.schema.isEmpty() && !schemas.contains(table.schema)) {
     queries.add("CREATE SCHEMA " + table.schema.trim() + ";");
     schemas.add(table.schema.trim());
     }

     MetaTable mtable = mtables.getTable(table.dbTable);

     if (mtable == null)
     queries.add(table.getQuery(false));

     for (HColumn col : table.columns) {
     MetaColumn mcolumn = mtable != null ? mtable.getColumn(col.name)
     : null;
     boolean alter = mcolumn != null && !col.dbCol.type().equalsIgnoreCase(mcolumn.type);
     if (mcolumn == null || alter)
     queries.add(col.getQuery(alter));
     }

     if (mtable != null)
     for (MetaColumn mcolumn : mtable) {
     boolean exists = false;
     for (HColumn col : table.columns)
     exists |= col.name.equalsIgnoreCase(mcolumn.name);

     if (!exists)
     queries.add("ALTER TABLE " + table.schema + "." + table.name + "\n"
     + "DROP COLUMN " + mcolumn.name);

     }

     }

     if (!queries.isEmpty())
     execute(queries.toString("\n;\n"));
     }
     */
    @Override
    public StrWriter escape(StrWriter qry, QueryObject obj) {

        Object value = obj.getValue();

        if (value == null)
            return qry.append("null");

        if (value == Undefined.TYPE)
            return qry; // pomiń wartość

        if (!obj.needEscape())
            return qry.append(value.toString());

        try {

            List<Object> list = obj.isCollection() ? obj.asCollection() : null;

            if (list != null) {
                if (Boolean.TRUE.equals(obj.array()))
                    qry.append("(");

                boolean first = true;
                for (Object o : list) {
                    if (!first)
                        qry.append(", ");

                    qry.nextLevel(() -> {
                        escape(qry, new QueryObject(this, o));
                    });

                    first = false;
                }

                if (Boolean.TRUE.equals(obj.array()))
                    qry.append(")");

                return qry;
            }

            if (value instanceof java.util.Date)
                value = new SimpleDateFormat(dateFormat)
                        .format((java.util.Date) value);

            qry.append("'");
            qry.append(escapeSQL(Utils.toString(value)));
            qry.append("'");

        } finally {
            if (!Is.empty(obj.cast()))
                qry.append("::").append(obj.cast());
        }
        return qry;
    }

}

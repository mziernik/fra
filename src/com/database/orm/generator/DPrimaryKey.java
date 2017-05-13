package com.database.orm.generator;

import com.utils.collections.Strings;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

class DPrimaryKey {

    final List<DColumn> columns = new LinkedList<>();
    final Strings colNames = new Strings();
    final String name;

    DPrimaryKey(DMain gen, ResultSet rs) throws SQLException {

        name = rs.getString("pk_name");

        DSchema sch = gen.schemas.get(rs.getString("table_schem"));
        if (sch == null)
            return;

        DTable tbl = sch.tables.get(rs.getString("table_name"));
        if (tbl == null)
            return;

        DColumn c = tbl.columns.get(rs.getString("column_name"));

        if (c == null)
            return;

        if (tbl.pkey == null)
            tbl.pkey = this;

        tbl.pkey.columns.add(c);
        tbl.pkey.colNames.add(c.fieldName);

    }

}

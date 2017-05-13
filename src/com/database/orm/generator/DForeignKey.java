package com.database.orm.generator;

import java.sql.ResultSet;
import java.sql.SQLException;

class DForeignKey {

    final String name;
    DColumn local;
    DColumn foreign;

    DForeignKey(DMain gen, ResultSet rs) throws SQLException {

        //    printMeta(rs);
        name = rs.getString("fk_name");

        DSchema sch = gen.schemas.get(rs.getString("pktable_schem"));
        if (sch == null)
            return;
        DTable ftbl = sch.tables.get(rs.getString("pktable_name"));
        if (ftbl == null)
            return;
        foreign = ftbl.columns.get(rs.getString("pkcolumn_name"));

        sch = gen.schemas.get(rs.getString("fktable_schem"));
        if (sch == null)
            return;

        ftbl = sch.tables.get(rs.getString("fktable_name"));

        if (ftbl == null)
            return;

        local = ftbl.columns.get(rs.getString("fkcolumn_name"));
        ftbl.foreignKeys.add(this);
    }

}

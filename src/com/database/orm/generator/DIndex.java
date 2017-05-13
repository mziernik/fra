package com.database.orm.generator;

import com.utils.collections.Strings;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

class DIndex {

    final List<DColumn> columns = new LinkedList<>();
    final Strings colNames = new Strings();
    final String name;

    DIndex(ResultSet rs) throws SQLException {
        name = rs.getString("index_name");
    }

}

package com.database;

import com.database.drivers.postgresql.PostgreSQL;
import com.lang.LDatabase;
import java.sql.SQLException;

public class DbHandler {

    public DBConnectionData getConnectionData(Database db) throws SQLException {
        throw new SQLException(LDatabase.LACK_OF_DEFAULT_CONNECTION_PROFIL.toString());
    }

    public PostgreSQL getPgConnection(Object... args) throws SQLException {
        throw new SQLException(LDatabase.LACK_OF_DEFAULT_CONNECTION.toString());
    }

    public Database getDatabase() {
        throw new UnsupportedOperationException();
    }
}

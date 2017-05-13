package com.database.service;

import com.context.AppContext;
import com.context.unit_test.FraUnitTestContext;
import com.database.*;
import com.database.drivers.h2.H2;
import com.mlogger.Log;
import com.resources.core.Resources;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import org.h2.tools.Server;

/**
 * @author Miłosz Ziernik
 * @date 06 listopada 2015
 * @encoding UTF-8
 */
public class EventsDB extends H2 {

    private static File file;
    private static Boolean emptyDatabase;
    static Server server;

    private static DBConnectionData getConnData() {
        file = AppContext.varPath.getFile("events");

        boolean unitTest = FraUnitTestContext.isRunning();

        if (unitTest)
            emptyDatabase = true;

        if (emptyDatabase == null) {
            File f = new File(file + ".mv.db");
            emptyDatabase = !f.exists() || f.length() == 0;
        }

        // W trybie testów jednostkowych utwórz bazę w ramie
        DBConnectionData conndata = unitTest
                ? new DBConnectionData(H2.class, "H2:Mem", "eventsDbTest")
                : new DBConnectionData(H2.class, "H2", file.toString().replace("\\", "/"));

        if (unitTest)
            conndata.properties.put("DB_CLOSE_DELAY", "-1"); // nie zamykaj bazy

        WDatabase.addDatabase(conndata);
        return conndata;
    }

    @Override
    protected void initialize() throws SQLException {
        if (emptyDatabase)
            try {
                Log.info("Tworzę strukturę bazy danych zdarzeń");
                execute(Resources.getF("/META-INF/fra/events.sql", false));
                emptyDatabase = false;
            } catch (IOException ex) {
                throw new SQLException(ex);
            }
    }

    public EventsDB() {
        super(getConnData());
    }

}

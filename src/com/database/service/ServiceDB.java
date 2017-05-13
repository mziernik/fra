package com.database.service;

import com.context.AppContext;
import com.context.unit_test.FraUnitTestContext;
import com.database.*;
import com.database.drivers.h2.H2;
import com.database.queries.builder.QueryBuilder;
import com.mlogger.Log;
import com.thread.TThread;
import com.utils.Unquoted;
import java.io.File;
import java.sql.SQLException;
import java.util.LinkedList;
import org.h2.tools.Server;

/**
 * @author Miłosz Ziernik
 * @date 06 listopada 2015
 * @encoding UTF-8
 */
public class ServiceDB extends H2 {

    private final static ServiceDbQueueTh thread = new ServiceDbQueueTh();

    private static File file;
    private static Boolean emptyDatabase;
    static Server server;

    private static DBConnectionData getConnData() {

        boolean unitTest = FraUnitTestContext.isRunning();

        if (file == null || emptyDatabase == null) {
            file = AppContext.varPath.getFile("service");

            if (unitTest)
                emptyDatabase = true;

            if (emptyDatabase == null) {
                File f = new File(file + ".mv.db");
                emptyDatabase = !f.exists() || f.length() == 0;
            }
        }

        // W trybie testów jednostkowych utwórz bazę w ramie
        DBConnectionData conndata = unitTest
                ? new DBConnectionData(H2.class, "H2:mem", "serviceDbTest")
                : new DBConnectionData(H2.class, "H2", file.toString().replace("\\", "/"));

        //ToDo: Sprawdzic czy zadziala:   conndata.properties.put("MODE", "PostgreSQL");
        if (unitTest)
            conndata.properties.put("DB_CLOSE_DELAY", "-1"); // nie zamykaj bazy

        //     conndata.autoCommit = true;
        //  conndata.properties.put("AUTOCOMMIT", "FALSE");
        WDatabase.addDatabase(conndata);
        return conndata;
    }

    @Override
    protected void initialize() throws SQLException {

        new DbStructure(getClass(), "/META-INF/fra/db/service/database.conf") {

            @Override
            protected void updateInfo(Database db, int rev, boolean create) throws Exception {
                super.updateInfo(db, rev, create);
                db.update("meta_data", "key = 'db.date'")
                        .arg("value", new Unquoted("CURRENT_TIMESTAMP"))
                        .execute();
            }

        }.process(emptyDatabase);

    }

    public ServiceDB queue(QueryBuilder query) {
        synchronized (thread.queue) {
            thread.queue.add(query);
            thread.queue.notify();
        }
        thread.start();
        return this;
    }

    public ServiceDB() {
        super(getConnData());
    }

}

class ServiceDbQueueTh extends TThread {

    final LinkedList<QueryBuilder> queue = new LinkedList<>();

    public ServiceDbQueueTh() {
        super("Service DB queue");
    }

    @Override
    protected void run() throws Exception {

        while (isRunning())
            try {

                QueryBuilder query = null;

                while (query == null) {
                    synchronized (queue) {
                        query = queue.poll();
                        if (query == null)
                            queue.wait(1000);
                    }

                    Sessions.updateSessions();

                }

                ServiceDB db = new ServiceDB();
                db.logsEnabled = AppContext.devMode;
                db.execute(query.toString());

            } catch (InterruptedException e) {
                return;
            } catch (Throwable e) {
                Log.error(e);
            }

    }

}

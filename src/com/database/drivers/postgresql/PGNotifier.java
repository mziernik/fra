package com.database.drivers.postgresql;

import com.database.DBConnectionData;
import com.thread.TThread;
import java.sql.*;
import org.postgresql.PGConnection;
import org.postgresql.PGNotification;

public class PGNotifier extends TThread {

    // po stronie bazy należy wywołać NOTIFY message, 'parametr'
    private final PostgreSQL db;
    public final PGNotifierMessage intf;
    public final String[] notifyMessages;

    public static interface PGNotifierMessage {

        public void onMessage(PGNotifier notifier, PGNotification notification);
    }

    public PGNotifier(PostgreSQL db, PGNotifierMessage intf, String... notifyMessages) throws SQLException {
        super("PG Listener");
        this.db = db;
        this.notifyMessages = notifyMessages;
        this.intf = intf;
        if (notifyMessages == null || notifyMessages.length == 0)
            return;
        start();
    }

    @Override
    protected void run() throws Exception {

        int errorCount = 0;
        while (isRunning())
            try {
                try (DBConnectionData.DbLock lock = db.getLock("LISTEN " + notifyMessages[0], 0)) {

                    PGConnection pgConn = (PGConnection) lock.connection;

                    try (Statement stmt = lock.connection.createStatement()) {
                        for (String s : notifyMessages)
                            stmt.execute("LISTEN " + s.trim());
                    }

                    PGNotification notifications[] = pgConn.getNotifications();
                    if (notifications != null)
                        for (PGNotification pgn : notifications)
                            intf.onMessage(this, pgn);
                }

                errorCount = 0;
                Thread.sleep(1000);

            } catch (InterruptedException e) {
                return;
            } catch (Exception e) {
                onException(e);
                ++errorCount;
                Thread.sleep(1000);
                if (errorCount > 10)
                    return;
            }

    }

}

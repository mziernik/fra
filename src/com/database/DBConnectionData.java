package com.database;

import com.utils.Utils;
import com.utils.Is;
import com.database.elements.JdbcDriver;
import com.lang.LDatabase;
import com.mlogger.Log;
import com.mlogger.status.ServiceMonitor;
import com.mlogger.status.StatusGroup;
import com.mlogger.status.StatusItem;
import com.servlet.pooling.*;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.sql.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Miłosz Ziernik 2014/03/26
 */
public class DBConnectionData {

    public final String url;
    public final String protocol;
    public final Class<? extends Database> driver;
    public final Properties properties = new Properties();
    public int lockTimeout;
    public boolean disconnectOnError = true;
    public boolean autoCommit = true;
    public String userName;
    public final String driverName;

    public DBConnectionData(Class<? extends Database> driver, String driverName, String url) {
        this.url = url;
        this.driver = driver;
        this.protocol = "jdbc:" + driverName.trim().toLowerCase(); // np jdbc:postgresql
        this.driverName = driverName;
    }

    @Override
    public String toString() {
        if (userName == null)
            return protocol + "://" + url;
        return protocol + "://" + userName + "@" + url;
    }

    public DBConnectionData property(String name, Object value) {
        if (name == null || value == null)
            return this;

        if (name.equalsIgnoreCase("user") || name.equalsIgnoreCase("username"))
            userName = value.toString();

        properties.put(name, value);
        return this;
    }

    public static class DbLock extends SingleConnection<Connection, Database, DBConnectionData, SQLException> {

        private final static String instanceId = Utils.randomId(4);
        private final static AtomicLong counter = new AtomicLong();
        public final String id;

        private final static StatusGroup sLocks = ServiceMonitor.service.group("db", "Database");

        private final StatusItem sts;

        public DbLock(ConnectionPool pool, Database sender,
                DBConnectionData data, int lockTimeout, String name) throws SQLException {
            super(pool, sender, DriverManager.getConnection(data.protocol + "://" + data.url, data.properties),
                    data, lockTimeout, name);

            synchronized (counter) {
                id = instanceId + "-" + counter.incrementAndGet();
            }

            sts = sLocks.item(id)
                    .value(data.toString())
                    .onUpdate((StatusItem arg) -> {
                        Long left = getLeftTime();
                        if (left != null)
                            arg.comment(new Interval(left, Unit.MILLISECONDS)
                                    .displayPrecision(Unit.SECONDS)
                                    .toString());
                    });

        }

        @Override
        public String toString() {
            return "DbLock " + id + ", " + connection.toString();
        }

        @Override
        public void doClose() throws SQLException {
            sts.remove();
            connection.close();
        }

        @Override
        protected void unlock(boolean timeout) throws SQLException {
            super.unlock(timeout);
            try {
                if (timeout && sender != null && sender.transaction != null) {
                    Log.warning("SQL", "Wycofuję transakcję " + lockName);
                    sender.rollbackTransaction();
                }
            } finally {
                if (sender.transaction != null && !sender.transaction.connection.isClosed())
                    sender.transaction.connection.setAutoCommit(true);
            }
        }

        @Override
        public boolean isSame(Database sender, DBConnectionData d) {
            // jeśli jest to transakcja to nie wykorzystuj tego połączenia
            if (sender.transaction != null)
                return false;

            return config.driver == d.driver
                    && config.toString().equals(d.toString())
                    && config.properties.equals(d.properties);
        }

    }

    public static class DbConnections extends ConnectionPool<DbLock, Database, DBConnectionData, SQLException> {

        private static Set<String> initialized = new HashSet<>();

        public DbConnections() {
            super(LDatabase.DATABASE.toString(), 600000, 10, 100);
        }

        @Override
        protected DbLock createConnection(Database sender, DBConnectionData data, int lockTimeout) throws SQLException {
            String clsName = data.driver.getAnnotation(JdbcDriver.class).value();
            try {
                if (data.driver != null)
                    Class.forName(clsName).newInstance();
            } catch (Exception ex) {
                throw new SQLException(ex);
            }

            String s = data.driver.getName() + ", " + data.protocol + ", " + data.url;
            if (!initialized.contains(s)) {
                initialized.add(s);
                try {
                    sender.initialize();
                } catch (Throwable e) {
                    initialized.remove(s);
                    throw e;
                }
            }

            return new DbLock(this, sender, data, lockTimeout, data.url);
        }

        @Override
        protected void onClose(DbLock connection, DBConnectionData data) {

        }

        /*
         @Override
         protected Connection getConnection(Database sender,
         DBConnectionData data) throws SQLException {



         // DriverManager.setLoginTimeout(CDatabase.timeout.value());
         return DriverManager.getConnection(data.url, data.properties);

         }
         */
    }

}

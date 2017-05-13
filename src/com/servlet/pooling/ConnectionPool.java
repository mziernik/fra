package com.servlet.pooling;

import com.mlogger.Log;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

public abstract class ConnectionPool<TSingleConn extends SingleConnection, TSender, TConfig, TException extends Exception> {

    protected int maxConnections;
    protected int maxQueueSize;
    protected int connectionAliveTime; // np 300000 ( 5 minut )
    public final String name;

    final List<TSingleConn> connections = new LinkedList<>();

    public List<SingleConnection> getConnections() {
        List<SingleConnection> list = new LinkedList<>();
        synchronized (connections) {
            list.addAll(connections);
        }
        return list;
    }

    public ConnectionPool(String connectionPoolName, int connectionAliveTime, int maxConnections, int maxQueueSize) {
        PoolsManager.pools.add(this);
        this.connectionAliveTime = connectionAliveTime;
        this.name = connectionPoolName;
        this.maxQueueSize = maxQueueSize;
        this.maxConnections = maxConnections;
    }

    protected final List<SingleConnection> conns = new LinkedList<>();

    protected abstract TSingleConn createConnection(TSender sender, TConfig config, int lockTimeout) throws TException;

    @SuppressWarnings("unchecked")
    public TSingleConn lock(TSender sender, TConfig config, int lockTimeout, String lockName) throws TException {

        if (PoolsManager.instance == null) {
            PoolsManager.instance = new PoolsManager("ConnectionPool: " + lockName);
            PoolsManager.instance.start();
        }

        List<TSingleConn> conns = new LinkedList<>();
        synchronized (connections) {
            conns.addAll(connections);
        }

        TSingleConn conn = null; // preferowane
        TSingleConn conn2 = null; // opcjonalne

        long threadId = Thread.currentThread().getId();
        for (TSingleConn sc : conns)
            if (sc != null && sc.lockedBy == null && sc.isSame(sender, config)) {
                if (conn2 == null)
                    conn2 = sc;
                if (sc.lockThreadId == threadId)
                    conn = sc;
                break;
            }

        // jesli nie dopasowano polaczenia do watku to przypisz opcjonalne
        if (conn == null)
            conn = conn2;

        if (conn == null) {
            if (PoolsManager.logEvents())
                Log.event("Connection", "Otwieram połączenie " + config);

            conn = createConnection(sender, config, lockTimeout);

            if (conn == null)
                return null;
            synchronized (connections) {
                connections.add(conn);
            }
        }

        ++conn.accessCount;
        conn.sender = sender;
        conn.lastUpdate = System.currentTimeMillis();
        conn.lockedBy = sender;
        conn.lockTimestamp = System.currentTimeMillis();
        conn.lockName = lockName;
        conn.lockThreadId = threadId;
        conn.lockTimeout = lockTimeout;
        return conn;

    }

    @SuppressWarnings("unchecked")
    public void closeAll(boolean force) {

        List<TSingleConn> conns = new LinkedList<>();
        synchronized (connections) {
            conns.addAll(connections);
        }

        for (TSingleConn conn : conns)
            try {
                conn.disconnect(force);
            } catch (Exception ex) {
                Log.warning(ex);
            }

    }

    protected void onClose(TSingleConn connection, TConfig config) {

    }

}

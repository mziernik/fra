package com.servlet.pooling;

import com.mlogger.Log;
import java.io.Closeable;
import java.io.IOException;

public abstract class SingleConnection<TConn, TSender, TConfig, TException extends Exception>
        implements Closeable {

    public final ConnectionPool pool;
    public final TConn connection;
    public final TConfig config;
    public final String connectionName;
    public TSender sender;
    Object lockedBy;
    long lockTimestamp;
    public String lockName;
    public long lockThreadId;
    boolean closeReuest;
    int lockTimeout;

    public final long created = System.currentTimeMillis();
    long lastUpdate = created;
    int accessCount;

    public abstract boolean isSame(TSender sender, TConfig config);

    public SingleConnection(ConnectionPool pool, TSender sender, TConn connection, TConfig config,
            int lockTimeout, String connectionName) {
        this.connection = connection;
        this.config = config;
        this.pool = pool;
        this.connectionName = connectionName;
        this.lockTimeout = lockTimeout;
        this.sender = sender;
    }

    public Long getLeftTime() {
        if (pool.connectionAliveTime <= 0)
            return null;
        long left = lastUpdate + pool.connectionAliveTime - System.currentTimeMillis();
        return left > 0 ? left : 0;
    }

    public int getAccessCount() {
        return accessCount;
    }

    @SuppressWarnings("unchecked")
    public void disconnect(boolean force) throws TException {
        if (!force && lockedBy != null) {
            closeReuest = true;
            return;
        }

        try {
            if (PoolsManager.logEvents())
                Log.event("Connection", "Zamykam połączenie " + config);
            doClose();
            pool.onClose(this, config);
        } finally {
            synchronized (PoolsManager.notifyObj) {
                PoolsManager.notifyObj.notifyAll();
            }
            synchronized (pool.connections) {
                pool.connections.remove(this);
            }

        }
    }

    protected abstract void doClose() throws TException;

    public void unlock() throws TException {
        unlock(false);
    }

    protected void unlock(boolean timeout) throws TException {
        lastUpdate = System.currentTimeMillis();
        lockedBy = null;
        lockName = null;
        lockTimestamp = 0;

        synchronized (PoolsManager.notifyObj) {
            PoolsManager.notifyObj.notifyAll();
        }
    }

    @Override
    public void close() throws IOException {
        try {
            unlock();
        } catch (Throwable e) {
            throw new IOException(e);
        }
    }
}

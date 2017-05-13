package com.servlet.pooling;

import com.config.CDebug;
import com.thread.TThread;

import com.config.engine.field.CfBoolAuto;
import com.context.AppContext;
import com.mlogger.Log;
import java.util.*;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.DEBUG__CONNECTION_EVENTS;

/**
 * Miłosz Ziernik 2013/10/27
 */
public class PoolsManager extends TThread {

    static PoolsManager instance;
    public final static List<ConnectionPool<?, ?, ?, ? extends Exception>> pools = new LinkedList<>();
    final static Object notifyObj = new Object();

    @Cfg(parent = CDebug.class)
    public final static CfBoolAuto logEvents = new CfBoolAuto("connection.events",
            DEBUG__CONNECTION_EVENTS);

    static boolean logEvents() {
        Boolean val = logEvents.value().bool;
        return val != null ? val : AppContext.devMode;
    }

    @SuppressWarnings("unchecked")
    public static void onServletContextDestroy() {
        for (ConnectionPool pool : pools)
            pool.closeAll(true);
    }

    public PoolsManager(String name) {
        super(name);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void run() throws Exception {

        while (isRunning())
            try {
                List<SingleConnection> toTermiate = new LinkedList<>();
                List<SingleConnection> toUnlock = new LinkedList<>();
                for (ConnectionPool pool : pools) {

                    List<SingleConnection> conns = new LinkedList<>();
                    conns.addAll(pool.connections);

                    for (Object sc : conns) {
                        if (!(sc instanceof SingleConnection))
                            continue;

                        SingleConnection conn = (SingleConnection) sc;

                        Long left = conn.getLeftTime();

                        if (conn.closeReuest || (left != null && left <= 0))
                            toTermiate.add(conn);

                        // zamknij polaczenie po przekroczenie limitu czasu blokady
                        if (conn.lockTimestamp > 0 && conn.lockTimeout > 0
                                && System.currentTimeMillis() > conn.lockTimestamp + conn.lockTimeout)
                            toUnlock.add(conn);
                    }

                }

                for (SingleConnection conn : toUnlock)
                    try {
                        if (logEvents())
                            Log.warning("Connection", "Odblokowuje połączenie \"" + conn.config + "\""
                                    + (conn.lockName != null && !conn.lockName.isEmpty() ? " zablokowane przez \""
                                    + conn.lockName + "\"" : "") + " (po upływie "
                                    + (System.currentTimeMillis() - conn.lockTimestamp) + " ms)");

                        conn.unlock(true);
                    } catch (Exception e) {
                        Log.error(e);
                    }

                for (SingleConnection conn : toTermiate)
                    try {
                        conn.disconnect(true);
                    } catch (Exception e) {
                        Log.error(e);
                    }

                synchronized (notifyObj) {
                    notifyObj.wait(100);
                }

            } catch (InterruptedException ie) {
                return;
            } catch (Exception e) {
                Log.error(e);
            }
    }
}

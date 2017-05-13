package com.database.async;

import com.utils.Utils;
import com.utils.Is;
import com.database.*;
import com.database.queries.MultipleQuery;
import com.database.queries.Query;
import com.mlogger.Log;
import com.thread.TThread;
import com.utils.date.time.Interval;
import java.util.*;
import java.util.Map.Entry;

public class AsyncDatabase<DB extends Database> {

    public final DB db;
    final LinkedHashMap<Object, AsyncQuery> queue = new LinkedHashMap<>();
    public int limit = 300; // maksymalna ilość zapytań wykonywanych jednocześnie

    public final TThread thread = new TThread("DB async executor") {

        @Override
        protected void run() throws Exception {

            while (isRunning())
                try {

                    LinkedList<AsyncQuery> list;
                    synchronized (queue) {
                        if (queue.isEmpty())
                            queue.wait();
                        list = procesQueue();
                    }

                    if (list.isEmpty())
                        continue;

                    MultipleQuery mqry = list.peekFirst().getQuery();

                    if (list.size() > 1) {
                        mqry = db.multipleQuery();
                        for (AsyncQuery aqry : list) {
                            mqry.add(new Query(db, "SELECT ? AS class_name, ? AS hash_code",
                                    aqry.getClass().getName(), aqry.hashCode()));
                            mqry.add(aqry.getQuery());
                        }
                    }

                    QueryRows rows = mqry.execute();
                    while (rows != null) {

                        if (list.size() == 1) {
                            AsyncQuery aqry = list.peekFirst();
                            if (aqry.callback != null)
                                aqry.callback.onResult(rows);
                            break;
                        }

                        QueryRow row = rows.firstD();

                        if (rows.size() == 1 && rows.columns.length == 2
                                && "class_name".equals(rows.columns[0].name)
                                && "hash_code".equals(rows.columns[1].name))

                            for (AsyncQuery aqry : list)
                                if (aqry.getClass().getName().equals(row.getStr("class_name"))
                                        && aqry.hashCode() == row.getInt("hash_code")) {
                                    if (aqry.callback != null)
                                        aqry.callback.onResult(rows.nextResults());
                                    break;
                                }
                        rows = rows.nextResults();
                    }

                } catch (Throwable e) {
                    AsyncDatabase.this.onException(e);
                } finally {
                    Thread.sleep(100);
                }

        }
    };

    protected LinkedList<AsyncQuery> procesQueue() {

        LinkedList<AsyncQuery> result = new LinkedList<>();

        long now = System.currentTimeMillis();

        // przetwórz priorytetowe żądania
        for (Entry<Object, AsyncQuery> en : new LinkedHashMap<>(queue).entrySet())
            if (en.getValue().priority) {
                result.add(en.getValue());
                queue.remove(en.getKey());
                return result;
            }

        for (Entry<Object, AsyncQuery> en : new LinkedHashMap<>(queue).entrySet()) {
            AsyncQuery qry = en.getValue();

            if (qry.created + qry.delay <= now) {
                queue.remove(en.getKey());
                result.add(qry);
                if (result.size() == limit)
                    return result;
            }
        }

        return result;
    }

    protected void onException(Throwable e) {
        Log.error(e);
    }

    public AsyncDatabase(DB db) {
        this.db = db;
    }

    void add(AsyncQuery qry) {
        synchronized (queue) {
            queue.put(Utils.coalesce(qry.context, new Object()), qry);
            queue.notify();
        };

        if (!thread.isRunning())
            thread.start();
    }

    /**
     * Dodaj zadanie do kolejki
     *
     * @param provider
     * @return
     */
    public AsyncQuery add(AsyncQueryProvider provider) {
        return add(null, null, provider, null);
    }

    public AsyncQuery add() {
        return add(null, null, null, null);
    }

    public AsyncQuery add(AsyncQueryProvider provider, AsyncQueryResult callback) {
        return add(null, null, provider, callback);
    }

    public AsyncQuery add(Object context, AsyncQueryProvider provider,
            AsyncQueryResult callback) {
        return add(context, null, provider, callback);
    }

    public AsyncQuery add(Object context, Interval delay,
            AsyncQueryProvider provider, AsyncQueryResult callback) {
        return new AsyncQuery(this, context, delay, false, provider, callback);
    }

    /**
     * Wykonaj zapytanie priorytetowo (z pominięciem kolejki)
     *
     * @param provider
     * @param callback
     * @return
     */
    public AsyncQuery insert(AsyncQueryProvider provider, AsyncQueryResult callback) {
        return insert(null, provider, callback);
    }

    public AsyncQuery insert() {
        return insert(null, null, null);
    }

    public AsyncQuery insert(Object context, AsyncQueryProvider provider,
            AsyncQueryResult callback) {
        return new AsyncQuery(this, context, null, true, provider, callback);
    }

}

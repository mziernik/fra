package com.cache;

import com.cache.intf.CachedDataAddListener;
import com.cache.intf.CachedDataExpireListener;
import com.cache.intf.CachedDataRemoveListener;
import com.mlogger.Log;
import com.servlet.views.ViewsManager;
import com.thread.TThread;
import java.io.IOException;
import java.util.*;

public class CacheManager extends TThread implements Iterable<CachedData> {

    public final static CacheManager instance = new CacheManager();
    final static Map<String, CachedData> items = new LinkedHashMap<>();

    private final static LinkedHashSet<CachedDataAddListener> addListeners = new LinkedHashSet<>();
    private final static LinkedHashSet<CachedDataRemoveListener> removeListeners = new LinkedHashSet<>();
    private final static LinkedHashSet<CachedDataExpireListener> expireListeners = new LinkedHashSet<>();

    private CacheManager() {
        super("CacheManager");
    }

    @Override
    protected void run() throws Exception {
        while (isRunning())
            try {
                Thread.sleep(1000);

                for (CachedData cd : getList())
                    if (cd.getLeaveTime() <= 0)
                        try {
                            if (cd.getRequestLock(null) == null
                                    && cd.getSessionLock(null) == null)
                                cd.delete();
                        } catch (Throwable e) {
                            Log.warning(e);
                        }

                ViewsManager.processExpired();

            } catch (InterruptedException e) {
                return;
            } catch (Throwable e) {
                Log.error(e);
            }

    }

    public int getCachedDatasCount() {
        return items.size();
    }

    public long getTotalSize() {
        long size = 0;
        for (CachedData item : this)
            size += item.length();
        return size;
    }

    boolean doRemove(CachedData item) throws IOException {
        if (item == null)
            return false;

        synchronized (removeListeners) {
            for (CachedDataRemoveListener listener : removeListeners)
                if (!listener.onCachedDataRemove(item))
                    return false;
        }

        synchronized (items) {
            items.remove(item.key);
        }
        return true;
    }

    CachedData get(String key) {
        CachedData item;
        synchronized (items) {
            item = items.get(key);
        }
        if (item != null)
            item.peak();
        return item;
    }

    CachedData add(CachedData item) {
        synchronized (addListeners) {
            for (CachedDataAddListener listener : addListeners)
                listener.onCachedDataAdd(item);
        }
        synchronized (items) {
            items.put(item.key, item);
        }
        return item;
    }

    public void addListener(CachedDataAddListener listener) {
        synchronized (addListeners) {
            addListeners.add(listener);
        }
    }

    public void addListener(CachedDataExpireListener listener) {
        synchronized (expireListeners) {
            expireListeners.add(listener);
        }
    }

    public void addListener(CachedDataRemoveListener listener) {
        synchronized (removeListeners) {
            removeListeners.add(listener);
        }
    }

    public LinkedList<CachedData> getList() {
        final LinkedList<CachedData> list = new LinkedList<>();
        synchronized (items) {
            list.addAll(items.values());
        }
        return list;
    }

    public LinkedList<CachedData> find(String source, String type, String name) {
        final LinkedList<CachedData> list = new LinkedList<>();
        synchronized (items) {
            for (CachedData cd : items.values()) {
                if (source != null && !source.equals(cd.source))
                    continue;
                if (type != null && !type.equals(cd.type))
                    continue;
                if (name != null && !name.equals(cd.name))
                    continue;
                list.add(cd);
            }
        }
        return list;
    }

    @Override
    public Iterator<CachedData> iterator() {
        return getList().iterator();
    }

}
